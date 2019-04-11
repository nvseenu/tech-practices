def find_maximum_value(arr, capacity=0):
    """
    Args
        arr -> array of tuple(int, int). First item in the tuple is value
            second one is weight
        capacity -> A target weight to which we need find a maximum value    

    """
    ratio = []
    for index , (value, weight) in enumerate(arr):
        ratio.append((value / weight, index)) 
    
    sorted(ratio, reverse=True)
    print(f"Sorted : {ratio}")

    result = []
    remaining_capacity = capacity
    sum_value = 0
    sum_weight = 0
    for r, index in ratio:
        value, weight = arr[index]
        if remaining_capacity > weight:
            remaining_capacity = remaining_capacity - weight
            result.append((value, weight))
        else:
           remaining_capacity =  weight - remaining_capacity
           result.append((remaining_capacity * value, remaining_capacity))
           break
    

    return result        





