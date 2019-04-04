package util

import (
	"testing"
)

func TestExtractUserName(t *testing.T) {

	tdata := []struct {
		desc     string
		email    string
		userName string
	}{
		{
			"Valid Email",
			"C-srini.nata@xyz.com",
			"Srini Nata",
		},
		{
			"Email starts with c- instead of C-",
			"c-srini.nata@xyz.com",
			"Srini Nata",
		},
		{
			"Valid Email but case sensitive",
			"C-Srini.Nata@xyz.com",
			"Srini Nata",
		},
		{
			"Email does not start with C-",
			"srini.nata@xyz.com",
			"Rubia Mussrath",
		},
		{
			"Email does not have .",
			"srini@xyz.com",
			"Srini",
		},
		{
			"Invalid Email",
			"srini.nata",
			"",
		},
		{
			"Empty Email",
			"",
			"",
		},
	}

	for _, td := range tdata {
		userName := ExtractUserName(td.email)
		if userName != td.userName {
			t.Errorf("For case: %s, Expected: %s, but got: %s", td.desc, td.userName, userName)
		}
	}

}
