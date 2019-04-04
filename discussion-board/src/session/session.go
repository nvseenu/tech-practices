// This package provides custom session implementations by using mux session.
// It provides two session implementations such as
// - Http Session
// - Flash Session.

// We use Http Session to store/retrieve user session data such as user's first name,
// last name, email id and groups she belongs to etc.

// Flash session keeps the data until it is read next. the monent it is read, the cookie
// will be deleted.
// We use flash session to keep temporary user specific data such as question form ,
// answer form etc

// We do not store any user specifc session data in db. Instead we just use cookies.
package session

import (
	"fmt"
	"net/http"

	"github.com/gorilla/sessions"
)

// This interface stores and retrieves the session data.
type Session interface {
	IsExpired(r *http.Request) bool
	IsValid(r *http.Request) bool
	Get(w http.ResponseWriter, r *http.Request, key string) (interface{}, error)
	Put(w http.ResponseWriter, r *http.Request, key string, value interface{}) error
	Delete(w http.ResponseWriter, r *http.Request, key string) error
}

// This interface provides flash session functionality to store temporary session data.
type FlashSession interface {
	Session
	// By default, when you call Get() method, It will delete the cookie after it is read.
	// Sometimes we want to delete the cookie conditionally.
	// Below method can read the value from flash cookie without delting it.
	GetWithoutDelete(w http.ResponseWriter, r *http.Request, key string) (interface{}, error)
}

type SessionImpl struct {
	name  string
	store sessions.Store
}

func NewSession(name, secret, path string, maxAge int) Session {
	store := sessions.NewCookieStore([]byte(secret))
	//Set cookie expiration time
	store.Options = &sessions.Options{
		Path:   path,
		MaxAge: maxAge,
	}
	return &SessionImpl{name, store}
}

func (s *SessionImpl) IsExpired(r *http.Request) bool {
	return !s.IsValid(r)
}

func (s *SessionImpl) IsValid(r *http.Request) bool {
	var session *sessions.Session
	var err error
	if session, err = s.store.Get(r, s.name); err != nil {
		return false
	}

	return !session.IsNew
}

func (s *SessionImpl) Get(w http.ResponseWriter, r *http.Request, key string) (interface{}, error) {
	var session *sessions.Session
	var err error
	if session, err = s.store.Get(r, s.name); err != nil {
		return nil, err
	}
	if user, ok := session.Values[key]; !ok {
		return nil, fmt.Errorf("Invalid session")
	} else {
		return user, nil
	}
}

func (s *SessionImpl) Put(w http.ResponseWriter, r *http.Request, key string, value interface{}) error {
	var session *sessions.Session
	var err error
	if session, err = s.store.Get(r, s.name); err != nil {
		return err
	}
	session.Values[key] = value
	session.Save(r, w)
	return nil
}

func (s *SessionImpl) Delete(w http.ResponseWriter, r *http.Request, key string) error {
	var session *sessions.Session
	var err error
	if session, err = s.store.Get(r, s.name); err != nil {
		return err
	}
	session.Options.MaxAge = -1
	session.Save(r, w)
	return nil
}

type FlashSessionImpl struct {
	name  string
	store sessions.Store
}

func NewFlashSession(name, secret, path string, maxAge int) FlashSession {
	store := sessions.NewCookieStore([]byte(secret))
	//Set cookie expiration time
	store.Options = &sessions.Options{
		Path:   path,
		MaxAge: maxAge,
	}
	return &FlashSessionImpl{name, store}
}

func (s *FlashSessionImpl) IsExpired(r *http.Request) bool {
	return !s.IsValid(r)
}

func (s *FlashSessionImpl) IsValid(r *http.Request) bool {
	var session *sessions.Session
	var err error
	if session, err = s.store.Get(r, s.name); err != nil {
		return false
	}

	return !session.IsNew
}

func (s *FlashSessionImpl) Put(w http.ResponseWriter, r *http.Request, key string, value interface{}) error {
	var session *sessions.Session
	var err error
	if session, err = s.store.Get(r, s.name); err != nil {
		return err
	}
	session.AddFlash(value, key)
	session.Save(r, w)
	return nil
}

func (s *FlashSessionImpl) Get(w http.ResponseWriter, r *http.Request, key string) (interface{}, error) {
	return s.get(w, r, key, true)
}

func (s *FlashSessionImpl) Delete(w http.ResponseWriter, r *http.Request, key string) error {
	return nil
}

func (s *FlashSessionImpl) get(w http.ResponseWriter, r *http.Request, key string, deleteCookie bool) (interface{}, error) {
	var session *sessions.Session
	var err error
	if session, err = s.store.Get(r, s.name); err != nil {
		return "", err
	}

	var val interface{}
	if vals := session.Flashes(key); len(vals) > 0 {
		//Flash values are added and live until they are read first.
		//Flash values are accumulated if they are not yet read.
		//Hence recent flash value is something we expect.
		val = vals[len(vals)-1]
	}

	if deleteCookie {
		session.Save(r, w)
	}
	return val, nil
}

func (s *FlashSessionImpl) GetWithoutDelete(w http.ResponseWriter, r *http.Request, key string) (interface{}, error) {
	return s.get(w, r, key, false)
}
