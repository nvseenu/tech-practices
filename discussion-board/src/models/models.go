package models

import (
	"fmt"
	"time"

	"context"

	"gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
)

// error struct represents an error occurred during db operation.
type ModelError struct {
	Code    string
	Message string
}

func (e *ModelError) Error() string {
	return fmt.Sprintf("ModelError [ Code: %s and Message : %s ]", e.Code, e.Message)
}

func newModelError(code, msgformat string, args ...interface{}) error {
	s := fmt.Sprintf(msgformat, args...)
	return &ModelError{code, s}
}

type CategoryStats struct {
	Name           string `bson:"_id"`
	Description    string
	TotalQuestions int `bson:"totalQuestions"`
	TotalAnswers   int `bson:"totalAnswers"`
}

type SubCategoryStats struct {
	Name           string `bson:"_id"`
	Description    string
	Category       string `bson:"category"`
	TotalQuestions int    `bson:"totalQuestions"`
	TotalAnswers   int    `bson:"totalAnswers"`
}

// Fetch subcategory statistics such as total questions and answers belongs to given category
// by grouping them under each subcategory.
// It runs an aggregated query to collect such statistics.
func FetchSubCategoryStats(ctx context.Context, categoryName string) ([]*SubCategoryStats, error) {
	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)
	topics := session.DB("discussion-board").C("questions")
	pipe := topics.Pipe([]bson.M{

		bson.M{
			"$match": bson.M{
				"answers":  bson.M{"$exists": true},
				"category": categoryName,
			},
		},
		bson.M{
			"$project": bson.M{
				"subcategory":    1,
				"totalQuestions": 1,
				"totalAnswers":   bson.M{"$size": "$answers"},
			},
		},
		bson.M{
			"$group": bson.M{
				"_id":            "$subcategory",
				"totalQuestions": bson.M{"$sum": 1},
				"totalAnswers":   bson.M{"$sum": "$totalAnswers"},
			},
		},
	})

	scs := []*SubCategoryStats{}
	if err := pipe.All(&scs); err != nil {
		return nil, err
	}
	//Set category Name in the final result
	for _, sc := range scs {
		sc.Category = categoryName
	}
	return scs, nil
}

// Fetch category statistics such as total questions and answers belongs to each category.
// It runs an aggregated query to collect such statistics.
func FetchCategoryStats(ctx context.Context) ([]*CategoryStats, error) {
	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)
	topics := session.DB("discussion-board").C("questions")
	pipe := topics.Pipe([]bson.M{

		bson.M{
			"$match": bson.M{
				"answers": bson.M{"$exists": true},
			},
		},
		bson.M{
			"$project": bson.M{
				"category":       1,
				"totalQuestions": 1,
				"totalAnswers":   bson.M{"$size": "$answers"},
			},
		},
		bson.M{
			"$group": bson.M{
				"_id":            "$category",
				"totalQuestions": bson.M{"$sum": 1},
				"totalAnswers":   bson.M{"$sum": "$totalAnswers"},
			},
		},
	})

	cs := []*CategoryStats{}
	if err := pipe.All(&cs); err != nil {
		return nil, err
	}
	return cs, nil
}

type QuestionStats struct {
	Id           bson.ObjectId `bson:"_id"`
	IdStr        string
	Title        string
	Description  string
	Category     string
	SubCategory  string
	CreatedBy    *User     `bson:"created_by"`
	CreatedAt    time.Time `bson:"created_at"`
	TotalAnswers int       `bson:"totalAnswers"`
	RecentAnswer []*Answer `bson:"recentAnswer"`
}

// Fetch question statistics such as total questions and answers belongs to given category
// and subcategory.
// It runs an aggregated query to collect such statistics.
func FetchQuestionStats(ctx context.Context, category string, subcategory string) ([]*QuestionStats, error) {

	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)

	qc := session.DB("discussion-board").C("questions")
	pipe := qc.Pipe([]bson.M{
		bson.M{
			"$match": bson.M{
				"answers":     bson.M{"$exists": true},
				"category":    bson.M{"$eq": category},
				"subcategory": bson.M{"$eq": subcategory},
			},
		},
		bson.M{
			"$project": bson.M{
				"_id":          1,
				"category":     1,
				"subcategory":  1,
				"title":        1,
				"description":  1,
				"created_by":   1,
				"created_at":   1,
				"totalAnswers": bson.M{"$size": "$answers"},
				"recentAnswer": bson.M{"$slice": []interface{}{"$answers", -1}},
			},
		},
	})

	qs := []*QuestionStats{}
	if err := pipe.Iter().All(&qs); err != nil {
		return nil, err
	}
	return qs, nil
}
