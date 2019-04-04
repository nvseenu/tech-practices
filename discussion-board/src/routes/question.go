package routes

import (
	"models"
	"net/http"
	"strconv"
	"strings"
	"time"

	"github.com/gorilla/mux"
	"github.com/hoisie/mustache"
	log "github.com/sirupsen/logrus"
)

// Creates a new question with given details in the mongo db,
// and redirects the user to home page.
func createQuestion(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	cat := r.FormValue("category")
	subcat := r.FormValue("subcategory")
	title := r.FormValue("title")
	desc := r.FormValue("description")
	rlog.Debugf("Creating a new question with title:%s, category: %s and subcategory: %s in the db", title, cat, subcat)
	var user *models.User
	var err error
	if user, err = getUserFromSession(w, r, discBoardSession); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Fetched user details: %v from the session", user)
	q := &models.Question{
		Title:       title,
		Category:    cat,
		SubCategory: subcat,
		Description: desc,
		Answers:     make([]*models.Answer, 0),
		CreatedBy:   user,
		CreatedAt:   time.Now(),
	}
	if err := models.CreateQuestion(r.Context(), q); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Infof("Created a new question with id: %s", q.Id)
	http.Redirect(w, r, "/", http.StatusSeeOther)
	rlog.Infof("Redirected the user: %s to home page", user.Name)
}

// Represents a question thread containing question and its answers.
type QuestionView struct {
	Id           string
	Title        string
	Description  string
	Category     string
	SubCategory  string
	CreatedBy    *models.User
	CreatedAt    string
	Answers      []*AnswerView
	Editable     bool // Whether the user is allowed to edit the question
	UserName     interface{}
	ErrorMessage interface{}
}

//Reresents an answer to a question.
type AnswerView struct {
	Id          int
	SerialNum   int
	Description string
	CreatedBy   *models.User
	CreatedAt   string
	Editable    bool // Whether the user is allowed to edit the answer
}

// Prepares a question thread which contains question and its answers , based on given question id.
func getQuestionThread(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	qid := vars["qid"]
	rlog.Debugf("Preparing a question thread page for question id: %s", qid)
	var errorMessage interface{}
	msg := r.FormValue("errorMessage")
	if msg == "" {
		errorMessage = nil
	} else {
		errorMessage = msg
		rlog.Debugf("The request has errorMessage param: %s", errorMessage)
	}

	q, err := models.FetchQuestion(r.Context(), qid)
	if err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Fetched a question: %s with its all answsers from db", qid)
	qview := &QuestionView{
		Id:           q.Id.Hex(),
		Title:        q.Title,
		Description:  q.Description,
		Category:     q.Category,
		SubCategory:  q.SubCategory,
		CreatedBy:    q.CreatedBy,
		CreatedAt:    formatTime(q.CreatedAt),
		Editable:     true,
		UserName:     getUserName(w, r),
		ErrorMessage: errorMessage,
	}

	answers := []*AnswerView{}
	for idx, a := range q.Answers {
		aview := &AnswerView{
			Id: idx,
			//UI renders description and replies with serial num starting 1.
			//In other words, UI renders question description with serial num 1
			// and first reply as 2 adn so on. hence we have to increment
			// post id by 2.
			SerialNum:   idx + 2,
			Description: a.Description,
			CreatedBy:   a.CreatedBy,
			CreatedAt:   formatTime(a.CreatedAt),
			Editable:    true,
		}
		answers = append(answers, aview)
	}
	qview.Answers = answers
	str := mustache.RenderFileInLayout("views/questionThread.html", "views/layout.html", qview)
	w.Write([]byte(str))
}

type QuestionStatView struct {
	Id           string
	Title        string
	Description  string
	Category     string
	SubCategory  string
	CreatedBy    *models.User
	CreatedAt    string
	TotalAnswers int
	RecentAnswer *AnswerView
}

// Prepares question stats like how many questions have been created under
// given category and sub category name.
func questionStatsHandler(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	catName := vars["categoryName"]
	subCatName := vars["subCategoryName"]
	rlog.Debugf("Preparing question stats page for given category: %s , and subcategory: %s ", catName, subCatName)
	qs, err := models.FetchQuestionStats(r.Context(), catName, subCatName)
	if err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Fetched %d questions for  given category: %s , and subcategory: %s ", len(qs), catName, subCatName)

	qviews := []*QuestionStatView{}
	for _, q := range qs {
		qview := &QuestionStatView{
			Id:           q.Id.Hex(),
			Title:        q.Title,
			Description:  q.Description,
			Category:     q.Category,
			SubCategory:  q.SubCategory,
			CreatedBy:    q.CreatedBy,
			CreatedAt:    formatTime(q.CreatedAt),
			TotalAnswers: q.TotalAnswers,
		}

		//When recent answer is missing, take question itself as recent answer.
		if len(q.RecentAnswer) == 0 {
			qview.RecentAnswer = &AnswerView{
				CreatedBy: q.CreatedBy,
				CreatedAt: formatTime(q.CreatedAt),
			}
		} else {
			qview.RecentAnswer = &AnswerView{
				CreatedBy: q.RecentAnswer[0].CreatedBy,
				CreatedAt: formatTime(q.RecentAnswer[0].CreatedAt),
			}
		}
		qviews = append(qviews, qview)
	}
	str := mustache.RenderFileInLayout("views/questionStats.html", "views/layout.html", map[string]interface{}{
		"UserName":      getUserName(w, r),
		"QuestionStats": qviews,
	})
	w.Write([]byte(str))
}

// User would like to see an answer form to add a new answer or edit exisitng answer.
// First it checks if user is allowed to see the answer form. In order to see the answer form,
// User should be either thread owner or belong to set of groups authroized to access given category.
// if the user fails in both case, he would see an error message on the question page.
func getAnswerForm(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	catName := vars["categoryName"]
	subCatName := vars["subCategoryName"]
	qid := vars["qid"]
	rlog.Debugf("Preparing an answer form for given question: %s, category: %s and subcategory: %s", qid, catName, subCatName)

	var err error
	var user *models.User
	var question *models.Question
	if user, err = getUserFromSession(w, r, discBoardSession); err != nil {
		sendErrorResponse(w, r, err)
	}
	rlog.Debugf("Fetched user details from the session: %v", user)
	if question, err = models.FetchQuestion(r.Context(), qid); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Fetched owner of given question: %s =>  %v", qid, question.CreatedBy)

	//Checks if the user is an owner of the thread.
	if strings.EqualFold(question.CreatedBy.Email, user.Email) {
		rlog.Infof("Authroized the user: %s to reply a question: %d, as the user is a question owner", user.Name, question.Id.Hex())
		var form *AnswerForm
		if form, err = getAnswerFormFromFlashSession(r, w, flashSession); err != nil {
			rlog.Debugf("Error while retrived reply form data from flash session => %v", err)
			form = &AnswerForm{}
		}
		rlog.Debugf("Retrieved AnswerForm from flash session as the user might have some data entered before authentication : %v", form)
		str := mustache.RenderFileInLayout(
			"views/answerForm.html",
			"views/layout.html",
			map[string]interface{}{
				"UserName":         user.Name,
				"HidePostQuestion": true,
				"Title":            question.Title,
				"Description":      question.Description,
				"Form":             form,
			})
		w.Write([]byte(str))
		return
	}

	cats, err := models.FetchCategories(r.Context())
	if err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Fetched all categories (%d) from the db", len(cats))
	category := findCategory(cats, catName)
	rlog.Debugf("Found a category: %v that matched with given category name: %v", category, catName)
	authorized := hasUserAuthroizedToReply(user.Groups, category.AuthorizedGroups)
	if authorized {
		rlog.Infof("Authorized the user: %s to reply as the user belongs to auhorized groups of category:", user.Name, catName)
		var form *AnswerForm
		if form, err = getAnswerFormFromFlashSession(r, w, flashSession); err != nil {
			rlog.Errorf("Error while retrived reply form data from flash session => %v", err)
			form = &AnswerForm{}
		}
		rlog.Debugf("Retrieved ReplyFormData from flash session => %v", form)
		str := mustache.RenderFileInLayout(
			"views/answerForm.html",
			"views/layout.html",
			map[string]interface{}{
				"UserName":         user.Name,
				"HidePostQuestion": true,
				"Title":            question.Title,
				"Description":      question.Description,
				"Form":             form,
			})
		w.Write([]byte(str))
	} else {
		rlog.Infof("Did not authorize the user: %s to reply as the user does NOT belongs to auhorized groups of category: %s", user.Name, catName)
		errorMessage := "You are not allowed to reply!!!"
		http.Redirect(
			w,
			r,
			"/discussion-board/categories/"+catName+"/subcategories/"+subCatName+"/questions/"+qid+"?errorMessage="+errorMessage,
			http.StatusSeeOther)
	}
}

type AnswerForm struct {
	Description string
}

// Creates a new answer for the given question id.
// Once it is successfull, redirects the user to the question page where she can see her answer.
func CreateNewAnswer(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	catName := vars["categoryName"]
	subCatName := vars["subCategoryName"]
	qid := vars["qid"]
	rlog.Debugf("Storing given answer that belongs to question id: %s, category: %s, subcategory: %s", qid, catName, subCatName)
	desc := r.FormValue("description")

	var user *models.User
	var err error
	if user, err = getUserFromSession(w, r, discBoardSession); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Fetched user details from the session: %v", user)

	a := &models.Answer{
		Description: desc,
		CreatedBy:   user,
		CreatedAt:   time.Now(),
	}

	if err := models.CreateAnswer(r.Context(), qid, a); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Infof("Created an answer record that belongs to question id: %s, category: %s, subcategory: %s", qid, catName, subCatName)
	url := "/discussion-board/categories/" + catName + "/subcategories/" + subCatName + "/questions/" + qid
	http.Redirect(w, r, url, http.StatusSeeOther)
	rlog.Infof("Redirected the user: %s to %s", user.Name, url)
}

type QuestionForm struct {
	Category    string
	SubCategory string
	Title       string
	Description string
}

// Prepares an question form
//
// if any such form is already stored in flash session, it takes and sends the form
// data to front end.
// Otherwise it creates a new form.
func getQuestionForm(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	var form *QuestionForm
	var err error
	rlog.Debugf("Preparing a question form")
	if form, err = getQuestionFormFromFlashSession(r, w, flashSession); err != nil {
		rlog.Errorf("Error while retrived question form data from flash session => %v", err)
		form = &QuestionForm{}
	}
	rlog.Debugf("Retreived question form from the flash session: %v", form)
	str := mustache.RenderFileInLayout("views/postQuestionForm.html", "views/layout.html", map[string]interface{}{
		"UserName":         getUserName(w, r),
		"HidePostQuestion": true,
		"Form":             form,
	})
	w.Write([]byte(str))
}

//Updates a description of a question associated with given id.
func updateQuestion(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	qid := vars["qid"]
	desc := r.FormValue("description")
	rlog.Debugf("Updating description for given question id: %s", qid)

	q := &models.Question{
		Description: desc,
	}

	if err := models.UpdateQuestionById(r.Context(), qid, q); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Infof("Updated given description to the question id: %s", qid)
	w.Write([]byte("OK"))
}

// Updates the answer associated with given question id and answer id
func updateAnswer(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	qid := vars["qid"]
	aid := vars["answerId"]
	rlog.Debugf("Updating given description to answer id: %s of question id: %s", aid, qid)
	answerId, err := strconv.Atoi(aid)
	if err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	desc := r.FormValue("description")
	if err := models.UpdateAnswerById(r.Context(), qid, answerId, &models.Answer{Description: desc}); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Infof("Updated given description to answer id: %s of question id: %s", aid, qid)
	w.Write([]byte("OK"))

}

// Below are status codes of whether the current user has authroized to edit question
const (
	NOT_AUTHENTICATED = "NOT_AUTHENTICATED"
	SESSION_EXPIRED   = "SESSION_EXPIRED"
	AUTHORIZED        = "AUTHORIZED"
	NOT_AUTHORIZED    = "NOT_AUTHORIZED"
)

// Checks whether current user is authorized to edit the question or not.
// In order to edit a question, user must be owner of the question.
func authorizedToEditQuestion(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	qid := vars["qid"]
	rlog.Debugf("Checking if current user is authorized to edit the question: %s", qid)
	var err error
	var user *models.User
	var question *models.Question

	if !discBoardSession.IsValid(r) {
		if authSession.IsValid(r) {
			rlog.Infof("Current user's session has expired. Hence the user can not edit the question: %s", qid)
			w.Write([]byte(SESSION_EXPIRED))

		} else {
			rlog.Infof("Current user is NOT authenticated. Hence the user can not edit question: %s", qid)
			w.Write([]byte(NOT_AUTHENTICATED))
		}
		return
	}

	if user, err = getUserFromSession(w, r, discBoardSession); err != nil {
		sendErrorResponse(w, r, err)
		return
	}

	rlog.Debugf("Retrived user details from the session: %v", user)
	if question, err = models.FetchQuestion(r.Context(), qid); err != nil {
		sendErrorResponse(w, r, err)
		return
	}

	fields := log.Fields{
		"OwnerEmail": question.CreatedBy.Email,
		"UserEmail":  user.Email,
		"UserName":   user.Name,
		"QuestionId": question.Id,
	}

	if strings.EqualFold(question.CreatedBy.Email, user.Email) {
		rlog.WithFields(fields).Infof("Authorized current user to edit the question")
		w.Write([]byte(AUTHORIZED))
	} else {
		rlog.WithFields(fields).Infof("User is NOT authorized to edit the question")
		w.Write([]byte(NOT_AUTHORIZED))
	}
}

//In order to edit an answer , current user must be an owner of it.
//The function checks the same.
func authorizedToEditAnswer(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	qid := vars["qid"]
	aidVal := vars["answerId"]
	rlog.Debugf("Checking if current user is authorized to edit the answer: %s of question: %s", aidVal, qid)

	var err error
	aid := 0
	if aid, err = strconv.Atoi(aidVal); err != nil {
		sendErrorResponse(w, r, err)
		return
	}

	var user *models.User
	var question *models.Question

	if !discBoardSession.IsValid(r) {
		if authSession.IsValid(r) {
			rlog.Infof("Current user's session has expired. Hence the user can not edit answer: %s of question: %s", aidVal, qid)
			w.Write([]byte(SESSION_EXPIRED))

		} else {
			rlog.Infof("Current user is NOT authenticated. Hence the user can not edit answer: %s of question: %s", aidVal, qid)
			w.Write([]byte(NOT_AUTHENTICATED))
		}
		return
	}
	if user, err = getUserFromSession(w, r, discBoardSession); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Retrived user details from the session: %v", user)
	if question, err = models.FetchQuestion(r.Context(), qid); err != nil {
		sendErrorResponse(w, r, err)
		return
	}

	fields := log.Fields{
		"OwnerEmail":    question.Answers[aid].CreatedBy.Email,
		"UserEmail":     user.Email,
		"UserName":      user.Name,
		"QuestionId":    question.Id,
		"AnswerId":      question.Answers[aid],
		"QuestionOwner": question.CreatedBy.Email,
	}

	if strings.EqualFold(question.Answers[aid].CreatedBy.Email, user.Email) {
		rlog.WithFields(fields).Infof("Current user is authorized edit the answer as she has posted it")
		w.Write([]byte(AUTHORIZED))
	} else {
		rlog.WithFields(fields).Infof("Current user is NOT authorized edit the answer as she has not posted it")
		w.Write([]byte(NOT_AUTHORIZED))
	}

}

func findCategory(cats []*models.Category, categoryName string) *models.Category {
	for _, category := range cats {
		if category.Name == categoryName {
			return category
		}
	}
	return nil
}

//Checks whether atleast one of the user grooups is matching with one of the authorized groups.
//If so , returns true otherwise false.
func hasUserAuthroizedToReply(userGroups []string, authorizedGroups []string) bool {
	userGroupMap := make(map[string]bool)
	for _, ug := range userGroups {
		userGroupMap[ug] = true
	}

	for _, ag := range authorizedGroups {
		if _, ok := userGroupMap[ag]; ok {
			return true
		}
	}

	return false

}
