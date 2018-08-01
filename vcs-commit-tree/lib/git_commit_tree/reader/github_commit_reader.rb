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
        bs = fetch_branches
        puts "branches  => #{bs}"
        bs.each do |b|
          cs = fetch_commits(b)
          cs.each {|c| yield c }
        end
      end

      private

      def fetch_branches
        refs = @github_client.fetch_refs         
        branch_refs = filter_branch_refs refs       
        branch_refs = fetch_and_set_dates(branch_refs)
        branch_refs = sort_by_date(branch_refs)        
        branches(branch_refs)
      end

      def fetch_commits(branch)        
        puts "Getting commits of a branch: #{branch.name} and #{branch.commit_id}"
        stop = false
        branch_commits = []
        until stop
          cs = @github_client.fetch_commits branch.commit_id 
          commits = cs.map { |c| commit(branch.name, c) }
          branch_commits.concat(commits)
          puts "Loaded commits: #{branch_commits.length}"
          if  commits.last.initial?
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
        refs.map do |r|
          branch(r)          
        end
      end

      def sort_by_date(refs)
        refs.sort { |a,b| a["date"] <=> b["date"]}
      end

      def branch(ref)
        ref_name = ref["ref"]
        ref_name = ref_name.sub(/refs\/heads\//, "")
        sha = ref["object"]["sha"]        
        GitCommitTree::Core::Branch.new(
          name: ref_name.to_sym, 
          date: ref["date"],
          commit_id: sha)
      end

      def commit(branch_name, raw_commit)
        id = raw_commit["sha"]
        committer = raw_commit["commit"]["committer"]
        message = raw_commit["commit"]["message"]
        date = committer["date"]
        email = committer["email"]
        parents = raw_commit["parents"].map { |p| p['sha'] }
        GitCommitTree::Core::Commit.new(
          id: id, 
          message: message,
          date: date, 
          branch: branch_name,
          author: email,
          parent_ids: parents)
      end

      def fetch_and_set_dates(refs)
         refs.map do |r|
            c = @github_client.fetch_commit(r["object"]["sha"])
            r["date"] = c["committer"]["date"]
            r
         end
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
      def fetch_refs
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

      def fetch_ref(ref_name)
        uri = URI(API_BASE_URL+ "/repos/#{@repo_user}/#{@repo_name}/git/#{ref_name}")
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
      def fetch_commits(sha)
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

      def fetch_commit(sha)
        uri = URI(API_BASE_URL+ "/repos/#{@repo_user}/#{@repo_name}/git/commits/#{sha}")
        body = response_body(uri)
        if body.key?('message') && body['message'].downcase == "not found"
          raise "Unable to fetch #{uri}" 
        else
          body
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
