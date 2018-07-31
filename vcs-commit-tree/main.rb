require_relative "./lib/git_commit_tree/core"
require_relative "./lib/git_commit_tree/reader"
require 'sinatra'
require 'json'

client = GitCommitTree::Reader::GithubClient.new("nvseenu", "C0ntr01@16", "nvseenu", "tech-practices")
reader = GitCommitTree::Reader::GithubCommitReader.new(client)
tbuilder = GitCommitTree::Core::TreeBuilder.new
tree = tbuilder.build(reader)

get '/' do
	puts "Loading file: #{File.join('views', 'home.html')}"
    File.read(File.join('views', 'home.html'))
end

get '/:branch/commits' do
   puts "Get commits for #{params[:branch]}"
  content_type :json
  commits = []
  tree.commits(params[:branch]).each do |c| 
     commits  << c.to_h
  end
  commits.to_json
end
