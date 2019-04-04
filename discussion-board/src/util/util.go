package util

import (
	"strings"
)

// Extracts username from given email id.
// For example, it extracts user name as Srini Nata
// from the email id C-srini.nata@xyz.com

func ExtractUserName(email string) string {

	email = strings.Trim(email, "")
	if len(email) == 0 {
		return ""
	}
	email = strings.ToLower(email)
	parts := strings.Split(email, "@")
	if len(parts) < 2 {
		return ""
	}

	name := parts[0]
	names := strings.Split(name, ".")

	fname := ""
	lname := ""
	if len(names) < 2 {
		fname = names[0]
	} else {
		fname = names[0]
		lname = names[1]
	}

	fname = strings.Replace(fname, "c-", "", -1)
	fname = strings.ToUpper(string(fname[0])) + fname[1:]

	if lname == "" {
		return fname
	}
	lname = strings.ToUpper(string(lname[0])) + lname[1:]

	return fname + " " + lname
}
