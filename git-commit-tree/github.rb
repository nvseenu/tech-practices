require_relative "lib/git_commit_tree/reader"
require_relative "lib/git_commit_tree/core"

USER_NAME = "xxxxxxxx"
PASSWORD ="xxxxxxxx"
client = GitCommitTree::Reader::GithubClient.new(USER_NAME, PASSWORD)
reader = GitCommitTree::Reader::GithubCommitReader.new(client, "nvseenu", "tech-practices")
tbuilder = GitCommitTree::Core::TreeBuilder.new
tree = tbuilder.build(reader)
puts "tree : #{tree}"
puts "Found branches: #{tree.branches.length}"
tree.branches.each do |b|
   puts "branch = #{b}"
   puts "---------------------------"
   commits = tree.commits(b)
   commits.each { |c| puts c}
end
