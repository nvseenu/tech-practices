package routes

import (
	"encoding/json"
	"models"
	"net/http"

	"github.com/gorilla/mux"
	"github.com/hoisie/mustache"
)

// Fetches all categories and returns them in json format.
func getCategories(w http.ResponseWriter, r *http.Request) {
	if cats, err := models.FetchCategories(r.Context()); err != nil {
		sendErrorResponse(w, r, err)
	} else {
		writeJsonResponse(w, r, cats)
		ReqLog(r).Infof("Returned %d categories as json", len(cats))
	}
}

// Fetches category statistics and returns them in json format.
func categoryStatHandler(w http.ResponseWriter, r *http.Request) {
	if cs, err := models.FetchCategoryStats(r.Context()); err != nil {
		sendErrorResponse(w, r, err)
	} else if jsonData, err := json.Marshal(cs); err != nil {
		ReqLog(r).Debugf("Fetched %d category stats from db", len(cs))
		sendErrorResponse(w, r, err)
	} else {
		w.Header().Set("Content-Type", "application/json")
		w.Write(jsonData)
	}
}

// Prepares a sub category statistics page for given category name.
// When the category has "None" as its sub category, it redirects
// the user to topic stats page of "None" category as  there are no subcategory stats
// for "None".
func SubCategoryStatsHandler(w http.ResponseWriter, r *http.Request) {
	rlog := ReqLog(r)
	vars := mux.Vars(r)
	catName := vars["categoryName"]
	rlog.Debugf("Fetching subcategory stats for given category: %s", catName)
	var scs []*models.SubCategoryStats
	var err error
	if scs, err = models.FetchSubCategoryStats(r.Context(), catName); err != nil {
		sendErrorResponse(w, r, err)
		return
	}
	rlog.Debugf("Fetched %d subcategory stats for given category: %s", len(scs), catName)

	// When a category has "None" as its subcategory, there is no subcategory stats to show to user.
	// Hence we redirect to topics page where usee can see the topic stats of "None" subcategory
	if len(scs) > 0 && scs[0].Name == "None" {
		url := r.URL.Path + "/None/topics"
		http.Redirect(w, r, url, http.StatusSeeOther)
		rlog.Infof("Redirected current url: %s to %s as given category %s has \"None\" as its subcategory", r.URL.Path, url, catName)
	} else {
		populateSubCategoryDescriptions(r, catName, scs)
		str := mustache.RenderFileInLayout("views/subCategoryStats.html", "views/layout.html",
			map[string]interface{}{
				"UserName":         getUserName(w, r),
				"SubCategoryStats": scs,
			})
		w.Write([]byte(str))
	}
}

// We prepare sub category stats by running aggregated query on "questions" collection.
// Question collection will contain sub category name, but not description.
// In order to display the description in the front end, we pick
// a corresponding description from the category map, and populates it.

// This function fecthes all categories first for each http request,
// and then finds out description for given sub category name.
// We can improve the performance by caching category collections.
func populateSubCategoryDescriptions(r *http.Request, catName string, scs []*models.SubCategoryStats) {
	if cats, err := models.FetchCategories(r.Context()); err == nil {
		smap := getSubCategoryMap(catName, cats)
		for _, stat := range scs {
			if scat, ok := smap[stat.Name]; ok {
				stat.Description = scat.Description
			}
		}
	}
}

func getSubCategoryMap(catName string, cats []*models.Category) map[string]models.SubCategory {
	m := make(map[string]models.SubCategory)
	for _, c := range cats {
		if c.Name == catName {
			for _, s := range c.SubCategories {
				m[s.Name] = s
			}
		}
	}
	return m
}
