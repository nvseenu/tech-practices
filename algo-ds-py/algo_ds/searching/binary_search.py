def search(arr, key):
    return  _search(arr, key, 0, len(arr)-1) 


def _search(arr, key, start, end):  
    if start > end:
        return -1    

    mid = start+ ((end-start) // 2)

    if arr[mid] > key:
        return _search(arr, key, start, mid-1)
    elif arr[mid] < key:
        return _search(arr, key, mid+1, end)
    else:
        return mid  