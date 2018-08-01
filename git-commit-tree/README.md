Git Commit Tree
==============
    This project is used to represent commit tree of a repository along with various branches. Once the tree is contructed, we can visualize the commits history.



Setup:
-----
1) Install ruby recent version. Ensure bin folder of ruby is added into your path.
2) Extract git-commit-tree.zip into local  
3) Go into the folder and execute the command
	"gem install sinatra"
4) Open the file git-commit-tree/main.rb, and find out the below lines:
USER_NAME = "xxxxxx"
PASSWORD = "xxxxxx"
   Update the variables with your github user name and password 	
   
5) Execute the command
	"ruby main.rb"	 
   The above command starts a webserver in port 4567

6) Hit localhost:4567 in your browser. Now you can see a page contains "Repository" text box 
7) Enter repository name like nvseenu/techpractices
8) Now you can see a repository visualized.   	
9) You can reenter any repository. However, Github api has a limitation that we can 	fetch 100 commits at the maximum in one request. 
   Hence trying out a repository containing more than 500 commits would take a lot of time. So please try any repository that contains less than 500 commits.







    