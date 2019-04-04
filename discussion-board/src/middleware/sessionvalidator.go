package middleware

import (
	"net/http"
	"routes"
	"session"
	"strings"
)

// Creates a new http session validatior middleware.
// It checks if user is authenticated or not by checking their cookies.

// If the user is not authenticated or her session got expired, it redirectd the user
// to login page.
// This middleware also saves our route processing code from http session handling,
// as we dont have to write it separately in each route.

// For some url patterns, this middle ware does not do session validation.
// For example, static resources, login page.

// Additionaly, it detects whether the user session gets expired while submitting question form or answer form.
// Sending the user to login page and showing empty form makes the user frustrated. Because she
// needs to enter all data again.  For such requests, this middleware puts the form data
// into flash session. so that the form data can be taken back from there and presented to user
// after successful login.
//

func NewHttpSessionValidator(handler http.Handler, auth, dbs, flash session.Session) http.HandlerFunc {

	return func(w http.ResponseWriter, r *http.Request) {
		rlog := ReqLog(r)
		if strings.HasPrefix(r.URL.Path, "/discussion-board/static") {
			handler.ServeHTTP(w, r)
			return
		}

		rlog.Debug("Validating session")

		// Any one can access the urls which are not ending up eiter "questionForm"
		// and "answerForm"
		if !strings.HasSuffix(r.URL.Path, "questionForm") &&
			!strings.HasSuffix(r.URL.Path, "answerForm") {
			rlog.Debug("No validation required for the current url")
			handler.ServeHTTP(w, r)
			return
		}

		//Only authenticated user can access  urls ending up in "questionForm"
		// and "postForm"
		firstTimeUser := false
		if auth.IsExpired(r) {
			firstTimeUser = true
		}
		rlog.Debugf("Does the user visit our app firs time? %t", firstTimeUser)

		if dbs.IsValid(r) {
			handler.ServeHTTP(w, r)
			return
		}

		// If the session is expired, show "session expired" alert messgae
		// to the user
		errMsg := ""
		url := "/discussion-board/login?referencedBy=" + r.URL.Path
		if !firstTimeUser {
			errMsg = "Your session has expired. Please login again"
			url += "&errorMessage=" + errMsg
		}

		if strings.HasSuffix(r.URL.Path, "questionForm") && r.Method == "POST" {
			form := &routes.QuestionForm{
				r.FormValue("category"),
				r.FormValue("subcategory"),
				r.FormValue("title"),
				r.FormValue("description"),
			}
			if err := flash.Put(w, r, "QUESTION_FORM", form); err != nil {
				rlog.Error("Error => %v", err)
			} else {
				rlog.Debug("Saved QuestionForm : %v into flash session", form)
			}
		} else if strings.HasSuffix(r.URL.Path, "answerForm") && r.Method == "POST" {
			form := &routes.AnswerForm{
				r.FormValue("description"),
			}
			if err := flash.Put(w, r, "ANSWER_FORM", form); err != nil {
				rlog.Errorf("Error => %v", err)
			} else {
				rlog.Debugf("Saved AnswerForm : %v into flash session", form)
			}
		}
		rlog.Debugf("Found invalid session. Henc redirected incoming url :%s to %s", r.URL.Path, url)
		http.Redirect(w, r, url, http.StatusSeeOther)
	}
}
