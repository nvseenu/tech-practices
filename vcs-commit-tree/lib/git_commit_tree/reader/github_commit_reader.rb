require "net/http"
require "json"
require_relative "../core"

module GitCommitTree
  module Reader
    class GithubCommitReader
      def initialize(github_client)
        @github_client = github_client
      end

      def read
        br = all_branches_refs
        puts "branches refs => #{br}"
        br.each do |b|
          cs = commits(b)
          cs.each {|c| yield c }
        end
      end

      private

      def all_branches_refs
        refs = @github_client.refs 
        branch_refs = filter_branch_refs refs
        branches(branch_refs)
      end

      def commits(branch_ref)
        branch, sha = branch_ref
        puts "Getting commits of a branch: #{branch} and #{sha}"
        stop = false
        branch_commits = []
        until stop
          cs = @github_client.commits sha 
          commits = cs.map { |c| commit(branch, c) }
          branch_commits.concat(commits)
          puts "Loaded commits: #{branch_commits.length}"
          if  commits.last.initial_commit?
            stop = true
          else 
            sha = commits.last.parents[0]
          end          
        end
        branch_commits.reverse
      end

      def filter_branch_refs(refs)
        refs.select do |r|
          ref_name = r["ref"]
          ref_name.start_with? "refs/heads"
        end
      end

      def branches(refs)
        refs.each_with_object({}) do |r, acc|
          b = branch(r)
          acc[b[0]] = b[1]
        end
      end

      def branch(ref)
        ref_name = ref["ref"]
        ref_name = ref_name.sub(/refs\/heads\//, "")
        sha = ref["object"]["sha"]
        [ref_name.to_sym, sha]
      end

      def commit(branch_name, raw_commit)
        puts "raw_commit: #{raw_commit}" 
        id = raw_commit["sha"]
        committer = raw_commit["commit"]["committer"]
        message = raw_commit["commit"]["message"]
        date = committer["date"]
        email = committer["email"]
        parents = raw_commit["parents"].map { |p| p['sha'] }
        GitCommitTree::Core::Commit.new(id, message, date, branch_name, email, parents)
      end
    end

    #
    #  It connnects to github through github rest api and 
    #  fetch information related to a repository
    #
    class GithubClient
      API_BASE_URL = "https://api.github.com"

      def initialize(username, password, repo_user, repo_name)
        @username = username
        @password = password
        @repo_user = repo_user
        @repo_name = repo_name
        authenticate
      end

      def authenticate
        uri = URI(API_BASE_URL)
        req = Net::HTTP::Get.new(uri)
        req.basic_auth(@username, @password)
        res = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) {|http|
          http.request(req)
        }
        unless res.instance_of? Net::HTTPOK
          str = "Unable to authenticate given username: #{username} due to an errr: #{res} "
          raise str
        end
      end

      # Fetches all references from a repo
      def refs
        uri = URI(API_BASE_URL+ "/repos/#{@repo_user}/#{@repo_name}/git/refs")
        body = response_body(uri)
              
        if body.instance_of? Array
          body
        elsif body.key?('message') && body['message'].downcase == "not found"
          raise "Unable to fetch #{uri}" 
        else
          puts "WARNING unexpected body = #{body}"
          []  
        end
      end

      # Fetch a set of commits starts with given sha.
      def commits(sha)
        uri = URI(API_BASE_URL+ "/repos/#{@repo_user}/#{@repo_name}/commits?sha=#{sha}")
        body = response_body(uri)
        if body.instance_of? Array
          body
        elsif body.key?('message') && body['message'].downcase == "not found"
          raise "Unable to fetch #{uri}" 
        else
          puts "WARNING unexpected body = #{body}"
          []
        end
      end
      
      private

      # A helper method to call github rest api with basic authentication
      # and returns the response.
      def response_body(uri)
        req = Net::HTTP::Get.new(uri)
        req.basic_auth(@username, @password)
        resp = Net::HTTP.start(uri.hostname, uri.port, use_ssl: true) {|http|
          http.request(req)
        }
        JSON.parse resp.body
      end
    end
  end
end
