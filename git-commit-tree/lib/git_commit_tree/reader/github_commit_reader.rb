require "net/http"
require "json"
require_relative "../core"

module GitCommitTree
  module Reader

    #
    #  This class pulls branches and their respective commits
    #  of a given repository from github 
    #
    class GithubCommitReader

      # It represents a maximum number of commits we can fetch from github 
      # in one batch.
      COMMITS_PER_PAGE = 100

      def initialize(github_client, repo_user, repo_name)
        @github_client = github_client
        @repo_user = repo_user
        @repo_name = repo_name
      end

      #
      #  Fetches all branches and commits from given repository,  
      #  constructs custom commits and returns one by one.
      #
      def read
        bs = fetch_branches
        puts "branches  => #{bs}"
        bs.each do |b|
          cs = fetch_commits(b)
          cs.each {|c| yield c }
        end
      end

      private

      #
      # Fetches all branches of given repository and sorts them
      # based on their creation date. 
      #
      def fetch_branches
        refs = @github_client.fetch_refs(@repo_user,@repo_name)         
        branch_refs = filter_branch_refs refs       
        branch_refs = fetch_and_set_dates(branch_refs)
        branch_refs = sort_by_date(branch_refs)        
        branches(branch_refs)
      end

      #
      #  It fetches entires commits of given branch by using github client.
      #  if a branch contains more than 100 commits, it fetches them as batches repeatedly
      #  until it fetches everything.
      #  
      
      def fetch_commits(branch)        
        puts "Getting commits of a branch: #{branch.name} and #{branch.commit_id}"
        stop = false
        branch_commits = []
        commit_id = branch.commit_id
        until stop
          cs = @github_client.fetch_commits(@repo_user,@repo_name,commit_id, COMMITS_PER_PAGE)
          commits = cs.map { |c| commit(branch.name, c) }
          branch_commits.concat(commits)
          puts "Loaded commits: #{branch_commits.length}"          
          sha = commits.last.parent_ids[0]
          if commits.last.initial? || commit_id == sha
            stop = true
          else 
             commit_id = sha
          end        
        end
        branch_commits.reverse
      end

      #  
      #  References will contain pull requests and tags along with 
      #  branches. Since we support branches only, this method skips the remaining   
      #
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

      #  Fetches date of each reference 
      #  and returns them.    
      #
      def fetch_and_set_dates(refs)
         refs.map do |r|
            c = @github_client.fetch_commit(@repo_user, @repo_name, r["object"]["sha"])
            r["date"] = c["committer"]["date"]
            r
         end
      end
    end

    #
    #  This class connnects to github through github rest api and 
    #  fetch items such as refs, commits from a given repository
    #
    class GithubClient
      API_BASE_URL = "https://api.github.com"

      def initialize(username, password)
        @username = username
        @password = password        
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
          str = "Unable to authenticate given username: #{@username} due to an errr: #{res} "
          raise str
        end
      end

      # Fetches all references from given a repo
      def fetch_refs(repo_user,repo_name)
        uri = URI(API_BASE_URL+ "/repos/#{repo_user}/#{repo_name}/git/refs")
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

      #
      #  Fetches a specific reference 
      #  
      def fetch_ref(repo_user,repo_name, ref_name)
        uri = URI(API_BASE_URL+ "/repos/#{repo_user}/#{repo_name}/git/#{ref_name}")
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

      # Fetches a set of commits starts with given sha.
      def fetch_commits(repo_user,repo_name, sha, commits_size)
        uri = URI(API_BASE_URL+ "/repos/#{repo_user}/#{repo_name}/commits?sha=#{sha}&per_page=#{commits_size}")
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

      # Fetches a specific commit 
      def fetch_commit(repo_user,repo_name,sha)
        uri = URI(API_BASE_URL+ "/repos/#{repo_user}/#{repo_name}/git/commits/#{sha}")
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
