package models

import (
	"crypto/tls"
	"fmt"

	log "github.com/sirupsen/logrus"
	"gopkg.in/ldap.v2"
)

var ldapServer string
var baseDN string

// Keeps given ldap configuration into package level global variables.
func InitLDAP(config map[string]string) {
	ldapServer = config["url"]
	baseDN = config["baseDN"]
	log.Debugf("Initialized Ldap server: %s with baseDN: %s", ldapServer, baseDN)
}

type User struct {
	Name   string
	Email  string
	Groups []string
}

const (
	LDAP_ERROR                = "LDAP_ERROR"
	LDAP_USER_DONT_EXIST      = "LDAP_USER_DONT_EXIST"
	LDAP_UNABLE_TO_FIND_USER  = "LDAP_UNABLE_TO_FIND_USER"
	LDAP_TOO_MANY_USERS_FOUND = "LDAP_TOO_MANY_USERS_FOUND"
	LDAP_INVALID_LOGIN        = "LDAP_INVALID_LOGIN"
)

// Authenticates user by sending her crendentials to LDAP server.
// Upon successful authentication, it fetches user details such as first name, last name
// email id and groups she belongs to , from LDAP server.

// When authentication has failed, The function can detect what could be the reason from
// LDAP Server response, through it is not explicitly throwing errors except connection issue.
//
// For example, if LDAP response has no entries , it means it has no user with given name.
// If it has more than one entries, LDAP server has more than one users with the same name.
func AuthenticateAndGetUserInfo(username string, password string) (*User, error) {

	var conn *ldap.Conn

	conn, err := ldap.Dial("tcp", ldapServer)
	if err != nil {
		return nil, newModelError(LDAP_ERROR, "Unable to connect ldap server due to %s", err.Error())
	}
	defer conn.Close()

	// Reconnect with TLS
	err = conn.StartTLS(&tls.Config{InsecureSkipVerify: true})
	if err != nil {
		log.Fatal(err)
	}

	result, err := conn.Search(ldap.NewSearchRequest(
		baseDN,
		ldap.ScopeWholeSubtree,
		ldap.NeverDerefAliases,
		0,
		0,
		false,
		fmt.Sprintf("(&(objectClass=organizationalPerson)(uid=%s))", username),
		[]string{"dn"},
		nil,
	))

	if err != nil {
		log.Printf("Failed to find user: %s %s\n", username, err)
		return nil, newModelError(LDAP_UNABLE_TO_FIND_USER, "Failed to find user: %s due to the cause: %s", username, err.Error())

	} else if len(result.Entries) < 1 {
		log.Printf("User: %s does not exist\n", username)
		return nil, newModelError(LDAP_USER_DONT_EXIST, "User: %s does not exit", username)

	} else if len(result.Entries) > 1 {
		log.Printf("Too many entries returned for user: %s\n", username)
		return nil, newModelError(LDAP_TOO_MANY_USERS_FOUND, "Too many entries returned for user: %s", username)

	} else {
		if err := conn.Bind(result.Entries[0].DN, password); err != nil {
			log.Printf("Failed to authenticate user: %s\n", username)
			log.Printf("Reason: %s\n", err)
			return nil, newModelError(LDAP_INVALID_LOGIN, "Failed to authenticate the user: %s from ldap server due to the cause: %s", username, err.Error())
		} else {

			var user *User
			if user, err = fetchUser(conn, username); err != nil {
				return nil, newModelError(LDAP_UNABLE_TO_FIND_USER, "Unable to fetch user details for user: %s from ldap server due to the cause: %s", username, err.Error())
			}

			groups := []string{}
			if groups, err = fetchGroups(conn, username); err != nil {
				return nil, newModelError(LDAP_UNABLE_TO_FIND_USER, "Unable to fetch groups for user: %s due to the cause: %s", username, err.Error())

			}
			user.Groups = groups
			log.Printf("User: %s authenticated successfully and user detials  are is %v\n", username, user)
			return user, nil
		}
	}
	return nil, nil
}

func fetchUser(conn *ldap.Conn, username string) (*User, error) {
	// Search for the given username
	searchRequest := ldap.NewSearchRequest(
		baseDN,
		ldap.ScopeWholeSubtree, ldap.NeverDerefAliases, 0, 0, false,
		fmt.Sprintf("(&(objectClass=organizationalPerson)(uid=%s))", username),
		[]string{"dn", "mail", "givenName", "sn", "cn"},
		nil,
	)

	sr, err := conn.Search(searchRequest)
	if err != nil {
		return nil, err
	}

	if len(sr.Entries) != 1 {
		log.Fatal("User does not exist or too many entries returned")
	}
	log.Printf("Loadign user details......")
	for _, entry := range sr.Entries {
		log.Printf("DN= %v", entry.DN)
		for _, attr := range entry.Attributes {
			log.Printf("Name = %v, values = %v", attr.Name, attr.Values)
		}
	}

	fullName := ""
	for _, entry := range sr.Entries {
		fullName = entry.GetAttributeValue("cn")
	}

	email := ""
	for _, entry := range sr.Entries {
		email = entry.GetAttributeValue("mail")
	}
	u := &User{
		Name:  fullName,
		Email: email,
	}
	log.Printf("Loadded user details. email:%s", email)
	return u, nil
}

func fetchGroups(conn *ldap.Conn, username string) ([]string, error) {
	// Search for the given groups for username
	searchRequest := ldap.NewSearchRequest(
		baseDN,
		ldap.ScopeWholeSubtree, ldap.NeverDerefAliases, 0, 0, false,
		fmt.Sprintf("(memberUid=%s)", username),
		[]string{"cn"},
		nil,
	)

	sr, err := conn.Search(searchRequest)
	if err != nil {
		log.Fatal(err)
	}

	groups := []string{}
	for _, entry := range sr.Entries {
		groups = append(groups, entry.GetAttributeValue("cn"))
	}
	return groups, nil
}
