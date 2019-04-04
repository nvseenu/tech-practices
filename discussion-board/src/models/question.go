package models

import (
	"context"
	"fmt"
	"time"

	"gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
)

type Question struct {
	Id          bson.ObjectId `bson:"_id,omitempty"`
	Title       string
	Description string
	Category    string
	SubCategory string
	CreatedBy   *User     `bson:"created_by"`
	CreatedAt   time.Time `bson:"created_at"`
	Answers     []*Answer
}

type Answer struct {
	Description string
	CreatedBy   *User     `bson:"created_by"`
	CreatedAt   time.Time `bson:"created_at"`
}

// Inserts a given question into "questions" collection
func CreateQuestion(ctx context.Context, q *Question) error {

	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)

	c := session.DB("discussion-board").C("questions")
	if err := c.Insert(q); err != nil {
		return err
	}
	return nil
}

// Fetches the question with its answers associated with given id.
// The entire question along with its all answers is stores as single document in
// mongodb. Below query fetch them all in one shot.
func FetchQuestion(ctx context.Context, qid string) (*Question, error) {
	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)

	qc := session.DB("discussion-board").C("questions")
	query := qc.Find(bson.M{"_id": bson.ObjectIdHex(qid)})

	q := &Question{}
	if err := query.One(q); err != nil {
		return nil, err
	}
	return q, nil
}

// Updates the question associated with given question id
func UpdateQuestionById(ctx context.Context, qid string, q *Question) error {
	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)
	qc := session.DB("discussion-board").C("questions")
	return qc.UpdateId(
		bson.ObjectIdHex(qid),
		bson.M{
			"$set": bson.M{
				"description": q.Description,
			},
		})
}

// A question document contains array of answers.
// Below query appends given question into such array.
func CreateAnswer(ctx context.Context, id string, a *Answer) error {
	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)
	c := session.DB("discussion-board").C("questions")
	err := c.Update(
		bson.M{
			"_id": bson.ObjectIdHex(id),
		},
		bson.M{
			"$push": bson.M{

				"answers": bson.M{
					"description": a.Description,
					"created_by":  a.CreatedBy,
					"created_at":  a.CreatedAt,
				},
			},
		})

	return err

}

// Updates given answer associated with given id.
// The id field is not a separate identifier like question id.
// Since question has all of its answers in an array, the id field
// represents index of the array.
func UpdateAnswerById(ctx context.Context, qid string, answerId int, a *Answer) error {
	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)
	qc := session.DB("discussion-board").C("questions")
	return qc.UpdateId(
		bson.ObjectIdHex(qid),
		bson.M{
			"$set": bson.M{
				fmt.Sprintf("answers.%d.description", answerId): a.Description,
			},
		})
}
