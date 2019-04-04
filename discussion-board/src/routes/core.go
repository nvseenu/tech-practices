//Routes package defines all routes required for discussion board app.

package routes

import (
	"encoding/json"
	"fmt"
	"models"
	"net/http"
	"session"
	"time"

	"github.com/gorilla/mux"
	"github.com/hoisie/mustache"
	log "github.com/sirupsen/logrus"
)

var discBoardSession session.Session
var authSession session.Session
var flashSession session.FlashSession
var emailConfig map[string]string

// Helper function to create Entry structure which is embedded with REQ_ID
// When we use this entry structure, it will log REQ_ID additionally along with each
// log statement.
func ReqLog(r *http.Request) *log.Entry {
	return log.WithFields(log.Fields{
		"reqId": r.Context().Value("REQ_ID"),
	})
}

//Initializes all required routes and returns the Handler type.
func NewRouter(auth, dbs session.Session, flash session.FlashSession, emailCfg map[string]string) http.Handler {

	// We use below package level varaibles to share between functions.
	discBoardSession = dbs
	authSession = auth
	flashSession = flash
	emailConfig = emailCfg
	log.Debug("Found Email condfiguration: %v", emailConfig)

	router := mux.NewRouter()
	//Serve static assets like js , css files
	router.PathPrefix("/discussion-board/static/").Handler(http.StripPrefix("/discussion-board/static/", http.FileServer(http.Dir("static"))))
	router.HandleFunc("/", homePageHandler)
	router.HandleFunc("/discussion-board", homePageHandler)
	router.HandleFunc("/discussion-board/login", processloginForm).Methods("POST")
	router.HandleFunc("/discussion-board/login", loginPageHandler)
	router.HandleFunc("/discussion-board/login/sendToken", generateAndSendToken).Methods("POST")
	router.HandleFunc("/discussion-board/login/verifyToken", verifyToken).Methods("POST")
	router.HandleFunc("/discussion-board/logout", logoutHandler)
	router.HandleFunc("/discussion-board/questionForm", createQuestion).Methods("POST")
	router.HandleFunc("/discussion-board/questionForm", getQuestionForm)
	router.HandleFunc("/discussion-board/categories", getCategories)
	// Some of the category name has spaces in its value.
	// For example, one of the category name is "Spectrum platform / guide". We could not capture these type of
	// values with simple path variable like {categoryName}. Hrnce we use below regular expression.
	router.HandleFunc("/discussion-board/categories/{categoryName:.*}/subcategories", SubCategoryStatsHandler)
	router.HandleFunc("/discussion-board/categories/{categoryName:.*}/subcategories/{subCategoryName}/topics", questionStatsHandler)
	router.HandleFunc("/discussion-board/categories/{categoryName:.*}/subcategories/{subCategoryName}/questions/{qid}", getQuestionThread)
	router.HandleFunc("/discussion-board/categories/{categoryName:.*}/subcategories/{subCategoryName}/questions/{qid}/answerForm", CreateNewAnswer).Methods("POST")
	router.HandleFunc("/discussion-board/categories/{categoryName:.*}/subcategories/{subCategoryName}/questions/{qid}/answerForm", getAnswerForm)
	router.HandleFunc("/discussion-board/questions/{qid}", updateQuestion).Methods("PUT")
	router.HandleFunc("/discussion-board/questions/{qid}", getQuestionThread)
	router.HandleFunc("/discussion-board/questions/{qid}/authorizedToEdit", authorizedToEditQuestion)
	router.HandleFunc("/discussion-board/questions/{qid}/answers/{answerId}", updateAnswer).Methods("PUT")
	router.HandleFunc("/discussion-board/questions/{qid}/answers/{answerId}/authorizedToEdit", authorizedToEditAnswer)
	return router
}

// Prepares a home page with category stats.
func homePageHandler(rw http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	rlog.Debug("Preparing home page")
	var cs []*models.CategoryStats
	var err error
	if cs, err = models.FetchCategoryStats(r.Context()); err != nil {
		sendErrorResponse(rw, r, err)

	} else {
		rlog.Debugf("Fetched %d category stats", len(cs))
		rlog.Debugf("User name : %s", getUserName(rw, r))

		populateCategoryDescriptions(r, cs)
		str := mustache.RenderFileInLayout("views/home.html",
			"views/layout.html",
			map[string]interface{}{
				"UserName":      getUserName(rw, r),
				"CategoryStats": cs,
			})
		rw.Write([]byte(str))
	}
}

func formatTime(t time.Time) string {
	return t.Format("01/02/2006 03:04:05 pm")
}

func sendErrorResponse(rw http.ResponseWriter, r *http.Request, err error) {
	ReqLog(r).Debugf("Unable to process the route %s due to the error: %s", r.URL, err)
	http.Error(rw, "Internal Error", 500)
}

func writeJsonResponse(rw http.ResponseWriter, r *http.Request, data interface{}) {
	jsonData, err := json.Marshal(data)
	if err != nil {
		sendErrorResponse(rw, r, err)
	}
	rw.Header().Add("Content-Type", "application/json")
	rw.Write(jsonData)
}

// Below functions facilitate storing / retrieving values to/from sessions.
func getUserFromSession(w http.ResponseWriter, r *http.Request, s session.Session) (*models.User, error) {
	if userval, err := s.Get(w, r, "user"); err != nil {
		return nil, fmt.Errorf("Unable to retrieve the user keyword from the session")
	} else if user, ok := userval.(*models.User); !ok {
		return nil, fmt.Errorf("Unable to type assert the user from the session")
	} else {
		return user, nil
	}
}

func getUserName(w http.ResponseWriter, r *http.Request) interface{} {

	if user, err := getUserFromSession(w, r, discBoardSession); err != nil {
		return nil
	} else {
		return user.Name
	}
}

func putUserIntoSession(w http.ResponseWriter, r *http.Request, s session.Session, user *models.User) error {
	return s.Put(w, r, "user", user)
}

// Flash sessions are used to store the forms data such as question form and reply form temporarily.
// So that if user's session is expired while she is editing, the data will be preserved until she logins again.
// Once she logged in, her data will be presented to her and the temporary data will be removed.
func putTokenFormIntoFlashSession(r *http.Request, w http.ResponseWriter, s session.Session, form *TokenForm) error {
	return s.Put(w, r, "TOKEN_FORM", form)
}

func getTokenFormFromFlashSession(r *http.Request, w http.ResponseWriter, s session.FlashSession) (*TokenForm, error) {
	if formVal, err := s.GetWithoutDelete(w, r, "TOKEN_FORM"); err != nil {
		return nil, err
	} else if form, ok := formVal.(*TokenForm); !ok {
		return nil, fmt.Errorf("Unable to retrieve token form data from flash session")
	} else {
		return form, nil
	}
}

func deleteTokenFormFromFlashSession(r *http.Request, w http.ResponseWriter, s session.FlashSession) (*TokenForm, error) {
	//Calling Get method on the flash session would delete the token form automatically.
	if formVal, err := s.Get(w, r, "TOKEN_FORM"); err != nil {
		return nil, err
	} else if form, ok := formVal.(*TokenForm); !ok {
		return nil, fmt.Errorf("Unable to retrieve token form data from flash session")
	} else {
		return form, nil
	}
}

func getQuestionOwnerFromFlashSession(r *http.Request, w http.ResponseWriter, s session.Session) (string, error) {
	if ownerval, err := s.Get(w, r, "QUESTION_OWNER"); err != nil {
		return "", err
	} else if owner, ok := ownerval.(string); !ok {
		return "", fmt.Errorf("Unable to retrieve the question owner from the session")
	} else {
		return owner, nil
	}
}

func putQuestionOwnerIntoFlashSession(r *http.Request, w http.ResponseWriter, s session.Session, owner string) error {
	return s.Put(w, r, "QUESTION_OWNER", owner)
}

func getQuestionFormFromFlashSession(r *http.Request, w http.ResponseWriter, s session.Session) (*QuestionForm, error) {
	if formVal, err := s.Get(w, r, "QUESTION_FORM"); err != nil {
		return nil, err
	} else if form, ok := formVal.(*QuestionForm); !ok {
		return nil, fmt.Errorf("Unable to retrieve post question form data from flash session")
	} else {
		return form, nil
	}
}

func putQuestionFormIntoFlashSession(r *http.Request, w http.ResponseWriter, s session.Session, data *QuestionForm) error {
	return s.Put(w, r, "QUESTION_FORM", data)
}

func getAnswerFormFromFlashSession(r *http.Request, w http.ResponseWriter, s session.Session) (*AnswerForm, error) {
	if formVal, err := s.Get(w, r, "ANSWER_FORM"); err != nil {
		return nil, err
	} else if form, ok := formVal.(*AnswerForm); !ok {
		return nil, fmt.Errorf("Unable to retrieve reply form data from flash session")
	} else {
		return form, nil
	}
}

// We prepare category stats by running aggregated query on "questions" collection.
// Question collection will contain category name, but not description.
// In order to display category description in the front end, we pick
// a corresponding description from the category map, and populates it.

// This function fecthes all categories first for each http request,
// and then finds out description for given category name.
// We can improve the performance by caching category collections.

func populateCategoryDescriptions(r *http.Request, cs []*models.CategoryStats) {
	if cats, err := models.FetchCategories(r.Context()); err == nil {
		cmap := getCategoryMap(cats)
		for _, c := range cs {
			if cat, ok := cmap[c.Name]; ok {
				c.Description = cat.Description
			}
		}
	}

}

// Creates a map with category name as key and returns it.
func getCategoryMap(cs []*models.Category) map[string]*models.Category {
	m := make(map[string]*models.Category)
	for _, c := range cs {
		m[c.Name] = c
	}
	return m
}
