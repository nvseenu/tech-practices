package middleware

import (
	"context"

	"net/http"
	"strings"
	"time"

	"github.com/rs/xid"
	log "github.com/sirupsen/logrus"
)

// Creates a new LogHandler middleware.
// The middleware recieves all incoming http requests before the corresponding route recieves,
// dumps request specific information like url, method etc into logs for ddebugging purpose.
// Once the request is processed, the middleware takes the control again
// and logs performance metrics such as time elapsed, status code of the request, etc.
func NewLogHandler(handler http.Handler) http.HandlerFunc {
	return func(rw http.ResponseWriter, r *http.Request) {
		st := time.Now()
		ctx := context.WithValue(r.Context(), "REQ_ID", xid.New().String())
		r = r.WithContext(ctx)
		reqStr := formatRequest(r)
		lrw := &logResponseWriter{rw, http.StatusOK}
		handler.ServeHTTP(lrw, r)
		ReqLog(r).WithFields(log.Fields{
			"elapsed": time.Since(st),
			"status":  lrw.statusCode,
			"url":     reqStr,
		}).Info("Processed url.")
	}
}

type logResponseWriter struct {
	http.ResponseWriter
	statusCode int
}

func (w *logResponseWriter) WriteHeader(code int) {
	w.statusCode = code
	w.ResponseWriter.WriteHeader(code)
}

//formatRequest returns a string that contains request details separatred by "|"
func formatRequest(r *http.Request) string {
	arr := []string{r.Method, r.URL.String(), r.Host}
	return strings.Join(arr, " | ")
}
