// Defines middleware functions that are configured before/after routes.
// Each http request is intercepted by a matching middleware, before the
// request is forwarded to corresponding route.
package middleware

import (
	"net/http"

	log "github.com/sirupsen/logrus"
)

func ReqLog(r *http.Request) *log.Entry {
	return log.WithFields(log.Fields{
		"reqId": r.Context().Value("REQ_ID"),
	})
}
