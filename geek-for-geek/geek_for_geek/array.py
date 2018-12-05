def find_largest_sum_in_contiguous_subarray(arr):
    """
    Given an array of n positive distinct integers. 
    The problem is to find the largest sum of contiguous increasing subarray in O(n) time complexity.
    """
    sum = 0
    max_sum = 0

    for a in arr:
        sum = sum + a
        if sum < 0:
            sum = 0

        if max_sum < sum:
            max_sum = sum 

    
    return max_sum           
