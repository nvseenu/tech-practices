package routes

import (
	"fmt"
	"math/rand"
	"models"
	"net/http"
	"net/smtp"
	"strconv"
	"time"
	"util"

	"github.com/hoisie/mustache"
)

// Prepares a login page
func loginPageHandler(rw http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	rlog.Debug("Preparing a login page")
	var msg interface{}
	errMsg := r.FormValue("errorMessage")
	if errMsg == "" {
		msg = nil
	} else {
		msg = errMsg
		rlog.Debugf("Found errorMessage param: %s", errMsg)
	}
	str := mustache.RenderFileInLayout(
		"views/login.html",
		"views/layout.html",
		map[string]interface{}{
			"HidePostQuestion": true, //"Post Question" should not be displayed for login page
			"ErrorMessage":     msg,
			"ReferencedBy":     r.FormValue("referencedBy"),
		})
	rw.Write([]byte(str))
}

// Authenticates an user. If it is successful, It redirects the user to the page she requested before authentication.
// if authentication is not successfull, It will show a login page with error message to  the user.
// Once the user is authenticated successfully, this function drops both Auth and Session Cookies.

// Discussion board app uses two session cookies to keep track of its users. First one is named as "Auth Cookie"
// and later one is "session" cookie.
// Lifetime of first cookie is longer than second one. the sole purpose of it is to keep track of user has
// logged in to our application atleast once or not. For example, when an incoming request comes from unauthenticated
// user, it could be due to session expiration or request comes from the new user.
// Both the cases, browser will not send cookies. How do we differenciate from exisitng users from new users?
// That is why we use Auth cookie.

// If Auth cookie is present, but session cookie is not present, then we can idenitfy it is a session expiration use case.
// If both are not present, it is a request comes from a new user.
// Session cookie is mainly used to keep user specific session data like her name, groups adn email id.
// Its life time will be very shorter than auth cookie, mostly between 30 mins to 1 hour.

func processloginForm(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	username := r.FormValue("username")
	password := r.FormValue("password")
	referencedBy := r.FormValue("referencedBy")
	rlog.Debugf("Validating credentails for the user:%s, who came from %s", username, referencedBy)

	var err error
	var user *models.User
	if user, err = models.AuthenticateAndGetUserInfo(username, password); err != nil {
		rlog.Errorf("Unable to authenticate the user due to %v", err)
		ldapErr := err.(*models.ModelError)
		errMsg := getErrorMessage(ldapErr)
		str := mustache.RenderFileInLayout(
			"views/login.html",
			"views/layout.html",
			map[string]interface{}{
				"HidePostQuestion": true,
				"ErrorMessage":     errMsg,
				"ReferencedBy":     referencedBy,
			})
		w.Write([]byte(str))
		return
	}

	// Presence of this cookie alone can tell if the user has logged in to discussion board
	// once or not. Just for sake of adding some data, we do the below key.

	if err = authSession.Put(w, r, "authenticated", true); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debug("Dropped auth cookie for the user:%s", username)

	// Sets user structure to the session cookie for later use.
	if err = discBoardSession.Put(w, r, "user", user); err != nil {
		sendErrorResponse(w, r, err)
		return
	}

	rlog.Infof("Authenticated the user: %s successfully and created a new session", username)
	http.Redirect(w, r, referencedBy, http.StatusSeeOther)
	rlog.Infof("Redirects the user to %s", referencedBy)
}

// Keeps token details such as email id and token
type TokenForm struct {
	Email string
	Token int
}

func generateAndSendToken(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	email := r.FormValue("email")
	rlog.Debugf("Generating a token and send it to a given email Id: %s", email)
	token := generateToken()
	rlog.Debugf("Generated token is %d", token)
	putTokenFormIntoFlashSession(r, w, flashSession, &TokenForm{email, token})
	title := "Token Login to Video Operations Engineering Discussion Board"
	content := mustache.RenderFile(
		"views/tokenEmail.txt",
		map[string]interface{}{
			"Token": token,
		})

	rlog.Debugf("Sending email to %s with title: %s and content: %s", email, title, content)
	err := sendEmail(
		emailConfig["from"],
		email,
		title,
		content,
	)
	if err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	w.Write([]byte("OK"))
	rlog.Infof("Sent an email to %s with token: %d", email, token)
}

func verifyToken(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	email := r.FormValue("email")
	tokenVal := r.FormValue("token")
	rlog.Debugf("Verifying email: %s and token: %s", email, tokenVal)

	token := 0
	var err error
	if token, err = strconv.Atoi(tokenVal); err != nil {
		sendErrorResponse(w, r, err)
		return
	}

	form, err := getTokenFormFromFlashSession(r, w, flashSession)
	rlog.Debugf("Retrieved token form from flash session: %v", form)

	if form == nil {
		sendErrorResponse(w, r, fmt.Errorf("No token form found in the flash session"))
		return
	}
	if email == form.Email && token == form.Token {
		rlog.Infof("Verified email: %s and token: %s successfully", email, tokenVal)

		user := &models.User{
			Name:   util.ExtractUserName(email),
			Groups: []string{},
			Email:  email,
		}
		rlog.Debugf("Created user data for non ipa user: %v", user)
		if err = discBoardSession.Put(w, r, "user", user); err != nil {
			sendErrorResponse(w, r, err)
			return
		}
		rlog.Infof("Created a new session for %s", email)
		deleteTokenFormFromFlashSession(r, w, flashSession)
		rlog.Debug("Deleted token form from flash session as it has been used for token verification")
		w.Write([]byte("OK"))
	} else {
		rlog.Infof("Could not authenticate the user as the user's email: %s and token: %s are invalid", email, tokenVal)
		w.Write([]byte("Invalid emailId and token"))
	}
}

const (
	HIGH = 99999
	LOW  = 10000
)

var initSeed bool = true

// This helper function generates a unique token between LOW and HIGH.
// This token will be sent to the user via email.
func generateToken() int {
	if initSeed {
		rand.Seed(time.Now().UnixNano())
		initSeed = false
	}
	return LOW + rand.Intn(HIGH-LOW)
}

// Helper function to send an email with given details.
func sendEmail(from string, to string, subject string, content string) error {

	auth := smtp.PlainAuth("", from, "", emailConfig["server"])

	body := "To:" + to + "\r\n" +
		"Subject: " + subject + " \r\n" +
		"Content-Type: text/plain" +
		"\r\n" +
		content +
		"\r\n\r\n"

	if err := smtp.SendMail(emailConfig["server"]+":25", auth, from, []string{to}, []byte(body)); err != nil {
		return err
	}
	return nil
}

func getErrorMessage(err *models.ModelError) string {
	msg := ""
	switch err.Code {
	case models.LDAP_INVALID_LOGIN:
		msg = "Invalid IPA username and password"
	case models.LDAP_USER_DONT_EXIST:
		fallthrough
	case models.LDAP_UNABLE_TO_FIND_USER:
		msg = "Username is not found"
	default:
		msg = "Internal Server Error"
	}
	return msg
}
