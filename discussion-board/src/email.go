package main

import (
	"fmt"
	"net/smtp"

	"github.com/domodwyer/mailyak"
)

type Email struct {
	Server   string
	Port     int
	From     string
	Password string
	To       []string
	Subject  string
	Body     string
}

func main() {

	email := &Email{
		Server:   "mail.xyz.com",
		Port:     25,
		From:     "",
		Password: "",
		To:       []string{},
		Subject:  "This is test email",
		Body:     "Please ignore this email.",
	}

	fmt.Printf("Email config : %V", email)
	if err := sendEmail(email); err != nil {
		panic(err)
	}
	fmt.Println("Email sent through smtp!")

	if err := sendEmailThroughMailyak(email); err != nil {
		panic(err)
	}
	fmt.Println("Email sent through mailyak")
}

func sendEmail(email *Email) error {

	auth := smtp.PlainAuth("", email.From, email.Password, email.Server)

	body := "To:" + email.To[0] + "\r\n" +
		"Subject: " + email.Subject + " \r\n" +
		"Content-Type: text/plain" +
		"\r\n" +
		email.Body +
		"\r\n\r\n"

	fmt.Printf("Server: %s\n", fmt.Sprintf("%s:%d", email.Server, email.Port))
	fmt.Printf("Auth: %V\n", auth)
	fmt.Printf("From: %V\n", email.From)
	fmt.Printf("To: %V\n", email.To)
	fmt.Printf("Body: %s\n", body)

	if err := smtp.SendMail(fmt.Sprintf("%s:%d", email.Server, email.Port), auth, email.From, email.To, []byte(body)); err != nil {
		return err
	}
	return nil
}

func sendEmailThroughMailyak(email *Email) {
	mail := mailyak.New(fmt.Sprintf("%s:%d", email.Server, email.Port), smtp.PlainAuth("", email.From, email.Password, email.Server))

	mail.To(email.To[0])
	mail.From(email.From)
	mail.FromName("Rubia")
	mail.Subject(email.Subject)

	// Or set the body using a string setter
	mail.Plain().Set(email.Body)

	buf, err := m.buildMime()
	if err != nil {
		return err
	}
	fmt.Printf("Body: %s\n", string(buf.Bytes()))
	fmt.Println(mail.String())

	// And you're done!
	if err = mail.Send(); err != nil {
		return err
	}
	return nil
}
