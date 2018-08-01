module GitCommitTree
  module Core
    #
    # This class represents a commit in git repository.
    #
    class Commit
      include Enumerable
      attr_reader :id, :message, :date, :branch, :author, :parent_ids

      def initialize(id:, message:, date:, branch:, author:, parent_ids: [])
        raise "Arg id is nil" if id.nil?
        raise "Arg message is nil" if message.nil?
        raise "Arg date is nil" if date.nil?
        raise "Arg branch is nil" if branch.nil? || branch.empty?
        raise "Arg author is nil" if author.nil? || author.empty?

        @id = id
        @message = message
        @date = date
        @branch = branch
        @author = author        
        @parent_ids = parent_ids        
      end

      #
      # Returns true if current one is a very first commit of a repository.
      # Otherwise returns false.
      # 
      def initial?
         @parent_ids.empty?
      end

      def <=>(other)
        @id <=> other.id
      end     

      def to_s
        "Commit { id: #{@id}, message: #{@message}, 
                  date: #{@date}, branch: #{@branch}, 
                  author: #{@author}, parent_ids: #{@parent_ids}"                  
      end

      #
      # This method is required to convert this object to json.
      #
      def to_h
        {id: @id,
         message: @message,
         date: @date,
         branch: @branch,
         author: @author,
         parent_ids: @parent_ids}         
      end     
    end
  end
end
