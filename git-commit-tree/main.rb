require_relative "./lib/git_commit_tree/core"
require_relative "./lib/git_commit_tree/reader"
require 'sinatra'
require 'json'

USER_NAME = "xxxxxx"
PASSWORD = "xxxxxx"
client = GitCommitTree::Reader::GithubClient.new(USER_NAME, PASSWORD)

repos = {}

get '/' do
	puts "Loading file: #{File.join('views', 'home.html')}"
    File.read(File.join('views', 'home.html'))
end

get '/:repo_user/:repo_name/branches' do
  repo_user = params[:repo_user]
  repo_name = params[:repo_name]
  puts "Loading repo: #{repo_user}/#{repo_name}..."
  reader = GitCommitTree::Reader::GithubCommitReader.new(client, repo_user, repo_name)
  tbuilder = GitCommitTree::Core::TreeBuilder.new
  tree = tbuilder.build(reader)
  repos["#{repo_user}/#{repo_name}"] = tree
  content_type :json  
  tree.branches.to_json
end

get '/:repo_user/:repo_name/:branch/commits' do
  repo_user = params[:repo_user]
  repo_name = params[:repo_name]
  puts "Get commits for #{params[:branch]}"
  content_type :json

  tree = repos["#{repo_user}/#{repo_name}"] 
  commits = []
  tree.commits(params[:branch]).each do |c| 
     commits  << c.to_h
  end
  commits.to_json
end
