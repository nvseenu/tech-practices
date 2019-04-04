package models

import (
	"context"

	"gopkg.in/mgo.v2"
	"gopkg.in/mgo.v2/bson"
)

type Category struct {
	Id               bson.ObjectId `bson:"_id,omitempty"`
	Name             string
	Description      string
	SubCategories    []SubCategory
	AuthorizedGroups []string `bson:"authorized_groups"`
}

type SubCategory struct {
	Name        string
	Description string
}

// Fetches entire category collection and returns them.
func FetchCategories(ctx context.Context) ([]*Category, error) {
	session := ctx.Value("MONGODB_SESSION").(*mgo.Session)

	tc := session.DB("discussion-board").C("categories")
	query := tc.Find(bson.M{})

	cats := []*Category{}
	if err := query.All(&cats); err != nil {
		return nil, err
	}
	return cats, nil
}
