def mergesort lst
    return _mergesort_ lst.dup  # 副作用で配列が壊れるので、複製を渡す
end
 
def _mergesort_ lst
    if (len = lst.size) <= 1 then
        return lst
    end
 
    # pop メソッドの返す値と副作用の両方を利用して、lst を二分する
    lst2 = lst.pop(len >> 1)
 
    return _merge_(_mergesort_(lst), _mergesort_(lst2))
end
 
def _merge_ lst1, lst2
    len1, len2 = lst1.size, lst2.size
    result = Array.new(len1 + len2)
    a, b = lst1[0], lst2[0]
    i, j, k = 0, 0, 0
    loop {
        if a <= b then
            result[i] = a
            i += 1 ; j += 1
            break unless j < len1
            a = lst1[j]
        else
            result[i] = b
            i += 1 ; k += 1
            break unless k < len2
            b = lst2[k]
        end
    }
    while j < len1 do
        result[i] = lst1[j]
        i += 1 ; j += 1
    end
    while k < len2 do
        result[i] = lst2[k]
        i += 1 ; k += 1
    end
    return result
end