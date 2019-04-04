// This package initilizes parts such as Logs, DB, LDAP Server, Cookies, Routes.
// Then it starts a web server.

package main

import (
	"encoding/gob"
	"encoding/json"
	"flag"
	"fmt"
	"io/ioutil"
	"middleware"
	"models"
	"net/http"
	"routes"
	"session"
	"strconv"

	log "github.com/sirupsen/logrus"
	"gopkg.in/mgo.v2"
	"gopkg.in/natefinch/lumberjack.v2"
)

//Configuration keywords that are used at the first nesting level
const (
	CONIFIG_FILE   = "config/app.json"
	DB             = "db"
	LOG            = "log"
	SESSION_COOKIE = "sessionCookie"
	AUTH_COOKIE    = "authCookie"
	FLASH_COOKIE   = "flashCookie"
	EMAIL          = "email"
)

func main() {

	//Command line flag "-p" for port. The default port 8000 will be used if no port is passed in commandline
	p := flag.Int("p", 8000, "server port number")
	flag.Parse()
	fmt.Println("Starting server at", *p)

	appConfig := loadConfig(CONIFIG_FILE)
	initLog(appConfig[LOG])
	mongoSession := initDB(appConfig[DB])
	models.InitLDAP(appConfig["ldapServer"])

	authSession, dbSession, flashSession := initCookies(appConfig)
	router := routes.NewRouter(authSession, dbSession, flashSession, appConfig[EMAIL])

	//Set Middlewares
	router = middleware.NewMongoDBSessionHandler(router, mongoSession)
	router = middleware.NewHttpSessionValidator(router, authSession, dbSession, flashSession)
	router = middleware.NewLogHandler(router)
	http.ListenAndServe(fmt.Sprintf(":%d", *p), router)
}

// loadConfig loads configurations from a json file.
//  A json structure for configuration supports only two nesting levels.
//  that is why we use its type as map[string]map[string]string

func loadConfig(fileName string) map[string]map[string]string {
	data, err := ioutil.ReadFile(fileName)
	if err != nil {
		panic(err)
	}
	var cfg map[string]map[string]string
	err = json.Unmarshal(data, &cfg)
	if err != nil {
		panic(err)
	}
	fmt.Println("Loaded config file : ", fileName)
	return cfg
}

//Inits logging with rolling file appender
func initLog(config map[string]string) {
	log.SetLevel(log.DebugLevel)
	log.SetFormatter(&log.TextFormatter{
		FullTimestamp:   true,
		TimestampFormat: "2006-01-02 15:04:05.999999",
	})
	log.SetOutput(&lumberjack.Logger{
		Filename:   fmt.Sprintf("%s/discussion-board.log", config["dir"]),
		MaxSize:    50, // megabytes
		MaxBackups: 10,
		MaxAge:     60, //days
	})
}

//InitDB intializes a db and checks if it can connect to db for
// subsequent operations.
func initDB(config map[string]string) *mgo.Session {
	mongoSession, err := mgo.Dial(config["connectionString"])
	if err != nil {
		panic(err)
	}
	log.Debugf("Initialized mongodb : %s", config["connectionString"])
	return mongoSession
}

//Cookie stores are created as per given configuration and returned the same.
func initCookies(config map[string]map[string]string) (session.Session, session.Session, session.FlashSession) {

	auth := session.NewSession("discussion-board-auth",
		config[AUTH_COOKIE]["secret"],
		"/",
		getExpiryTime(config[AUTH_COOKIE]["expiryTime"]))
	discboard := session.NewSession("discussion-board-session",
		config[SESSION_COOKIE]["secret"],
		config[SESSION_COOKIE]["path"],
		getExpiryTime(config[SESSION_COOKIE]["expiryTime"]))

	flash := session.NewFlashSession("discussion-board-flash",
		config[FLASH_COOKIE]["secret"],
		"/",
		getExpiryTime(config[FLASH_COOKIE]["expiryTime"]))

	// to store a complex datatype like user within a session
	gob.Register(&models.User{})
	gob.Register(&routes.QuestionForm{})
	gob.Register(&routes.AnswerForm{})
	gob.Register(&routes.TokenForm{})
	log.Debug("Intialized cookies")
	return auth, discboard, flash
}

func getExpiryTime(t string) int {
	if val, err := strconv.Atoi(t); err != nil {
		panic(err)
	} else {
		return val
	}
}
