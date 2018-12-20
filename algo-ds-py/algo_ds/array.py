def maximum_subarray_brute_force(arr):
    """
    A brute force approach to find a subarray that gives maximum sum.
    Time complexity is  O(n^2)    
    """
    
    max_start_index = None
    max_end_index = None
    max = 0 

    for i in range(len(arr)-1):
        sum = 0
        for j in range(i, len(arr)):
            sum +=  arr[j]
            if sum > max:
                max = sum
                max_start_index = i
                max_end_index = j

    return (max_start_index, max_end_index, max)


def maximum_subarray_divide_and_conquer(arr):
    return _maximum_subarray_divide_and_conquer(arr, 0, len(arr)-1)


def _maximum_subarray_divide_and_conquer(arr, start_index, end_index):
    if start_index >= end_index:
        return start_index, end_index, arr[start_index]

    mid = (end_index - start_index) // 2

    left_start, left_end, left_sum = _maximum_subarray_divide_and_conquer(arr, start_index, start_index + mid)    
    right_start, right_end, right_sum = _maximum_subarray_divide_and_conquer(arr, start_index + mid + 1, end_index)    
    cross_start, cross_end, cross_sum = _maximum_subarray_for_cross_section(arr, start_index, start_index+mid, end_index)    
    
    max_sum = 0
    if left_sum > cross_sum and left_sum > right_sum:
        return (left_start, left_end, left_sum)
    elif right_sum > cross_sum and right_sum > left_sum:
        return (right_start, right_end, right_sum)
    else:
        return (cross_start, cross_end, cross_sum)

def _maximum_subarray_for_cross_section(arr, start, mid, end):    
    left_sum = arr[mid]
    left_max_sum = left_sum
    left_start = mid
    for i in range(mid-1, start-1, -1):
        left_sum += arr[i]

        if left_sum > left_max_sum:
            left_max_sum = left_sum
            left_start = i

    

    right_sum = arr[mid+1]
    right_max_sum = right_sum
    right_end = mid+1
    
    for i in range(mid+2, end+1):
        right_sum += arr[i]

        if right_sum > right_max_sum:
            right_max_sum = right_sum
            right_end = i

    return (left_start, right_end, left_max_sum + right_max_sum)        



            
            









    












    

