package routes

import (
	"models"
	"net/http"

	"github.com/hoisie/mustache"
)

// Prepares a login page
func logoutHandler(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	rlog.Debug("Logging out a current user")

	var user *models.User
	var err error
	if user, err = getUserFromSession(w, r, discBoardSession); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Fetched user details: %v from the session", user)

	// Delete user session cookie
	if err = discBoardSession.Delete(w, r, "user"); err != nil {
		sendErrorResponse(w, r, err)
		return
	}

	str := mustache.RenderFileInLayout(
		"views/logout.html",
		"views/layout.html",
		map[string]interface{}{
			"HidePostQuestion": true,
		})
	w.Write([]byte(str))
}
