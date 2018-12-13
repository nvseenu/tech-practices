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


def left_rotate(arr, n):
    """
    Rotate elements in the array by given position

    Args:
    arr(array) -> Array of elements
    n(int) -> Number of positions to rotate

    Time complexity: O(n^ number of rotations) 
    """
    
    for _ in range(n):
        _one_left_rotation(arr)



def _one_left_rotation(arr):
    t = arr[0]    
    n = len(arr)
    for i in range(n-1):
        arr[i] = arr[i+1]        
    
    arr[n-1] = t


def left_rotate1(arr, d):
    """
    Rotate elements in the array by given position

    Args:
    arr(array) -> Array of elements
    d(int) -> Number of positions to rotate

    Time complexity: O(n) 
    Space complexity: O(d)
    """    

    # Create a temp array and copy first d elements
    tarr = arr[:d]
    n = len(arr)

    # Move remaining elemnts to d position front
    for i in range(d, n):
        arr[i-d] = arr[i]

    index = n-d    
    for a in tarr:
        arr[index] = a
        index += 1       



def merge(arr1, arr2):
    """
    Merge an array of size n into another array of size m+n

    """

    # Move elements to end of the array arr1
    index1 = len(arr1)-1
    index2 = index1

    for i in range(index1, -1, -1):
        
        if arr1[i]:            
            arr1[index2] = arr1[i]

            # Check if both indexes are not same. otherwise
            # we will be overwriting an element with None
            if index2 != i:
                arr1[i] = None

            index2 -= 1
       
            
    i = 0
    index1 = index2+1
    index2 = 0
    
    while index1 < len(arr1) and index2 < len(arr2):       

        if arr1[index1] < arr2[index2]:
            arr1[i] = arr1[index1]            
            index1 += 1
        else:
            arr1[i] = arr2[index2]
            index2 += 1               
            
        i += 1    

    
    # Copy remaining elements of arr1 if any
    while index1 < len(arr1):
        arr1[i] = arr1[index1]
        index1 += 1
        i += 1


    # Copy remaining elements of arr2 if any
    while index2 < len(arr2):
        arr1[i] = arr2[index2]
        index2 += 1
        i += 1    



    








    
        
                