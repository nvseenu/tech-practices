package middleware

import (
	"context"
	"net/http"
	"strings"

	"gopkg.in/mgo.v2"
)

const MONGODB_SESSION = "MONGODB_SESSION"

// Creates a new mongo db session handler middleware and returns  it.
//
// Ideally we should not open a new connection to mongodb upon each request as
// it is time consuming operation. The best practice is opening a connection to mongodb
// once and creates a new session for each request.

// We already opens a new connection in main package. This middleware opens a new session,
// and puts it into a request context and forwards to corresponding route.
// Once the request is processed, it closes the session.

// This middleware keeps our route processing code simple, as we don't have to write opening and
// closing the session in each route.
//
// But how do we know if request needs mongodb session or not. it is hard. But we know that
// we dont need mongodb session to server static resources.
// Hence this middleware does not open sessions for static resources.

func NewMongoDBSessionHandler(handler http.Handler, session *mgo.Session) http.HandlerFunc {

	return func(rw http.ResponseWriter, req *http.Request) {
		rlog := ReqLog(req)
		if strings.HasPrefix(req.URL.Path, "/discussion-board/static") {
			handler.ServeHTTP(rw, req)
			return
		}
		session1 := session.Copy()
		defer session1.Close()
		ctx := context.WithValue(req.Context(), MONGODB_SESSION, session1)
		req = req.WithContext(ctx)
		rlog.Debug("Opened a new mongo db session")
		handler.ServeHTTP(rw, req)
		rlog.Debug("Closed the mongo db session")
	}
}
