from functools import reduce
from operator import countOf
import os
from sys import exit
from random import getrandbits, randint, choice
from re import sub

# Seems like Python doesn't have a built-in binary_search function
# however, since the purpose of this is to test the WACC binary_search I've written
# I've found it wiser to use a highly-upvoted binary_search implementation in Python

# source: https://stackoverflow.com/a/212413
def binary_search(arr, tar, lo=0, hi=None):
    if hi is None:
        hi = len(arr) - 1
    while lo <= hi:
        mid = (lo + hi) // 2
        midval = arr[mid]
        if midval < tar:
            lo = mid + 1
        elif midval > tar:
            hi = mid - 1
        else:
            return mid
    return -1

# Function that returns the ANDing of two booleans
def bool_and(b1, b2):
    return (b1 and b2)

# Function that returns the ORing of two booleans
def bool_or(b1, b2):
    return (b1 or b2)


##### RELATIVE LIBRARY IMPORT PATH #####
path = "../../../../../../stdlib/array_utils.wacc"

##### DATA TO CHOOSE FROM DEPENDING ON THE TYPE #####
rand_per_type = {
    "int": "randint(-2147483648, 2147483647)",
    "bool": "str(bool(getrandbits(1))).lower()",
    "real_bool": "bool(getrandbits(1))",
    "char": "chr(choice(list(set(range(32, 127)) - set([34, 35, 39, 92]))))",
    "short_int": "randint(-3, 3)",
    "positive_short_int": "randint(1, 3)",
    "short_char": "chr(choice(list(set(range(88, 100)) - set([92]))))",
}

##### FUNCTION TO GENERATE PRINT TESTS #####
def print_arr(function_name, type, no_of_test_cases, file):
    arrs = list()

    for i in range(no_of_test_cases):
        temp_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type[type]))
        arrs.append(temp_arr)

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for arr in arrs:
        file.write(f"# arr = {arr}\n")

    file.write(
        f'\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\tcall {function_name}("arr", arr) ;\n'
    )

    for i in range(1, no_of_test_cases):
        file.write(f'\tarr = {arrs[i]} ;\n\tcall {function_name}("arr", arr) ;\n')


##### FUNCTION TO GENERATE COUNT TESTS #####
def count_arr(function_name, type, no_of_test_cases, file):
    arrs = list()
    to_count = list()

    for i in range(no_of_test_cases):
        temp_type = type if type == "bool" else "short_" + type
        temp_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type[temp_type]))
        arrs.append(temp_arr)
        to_count.append(eval(rand_per_type[temp_type]))

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for i in range(no_of_test_cases):
        file.write(f"# {countOf(arrs[i], to_count[i])}\n")

    padding = "'" if type == "char" else ""

    file.write(
        f"\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\tint count = call count_{type}_arr({padding}{to_count[0]}{padding}, arr) ;\n\tprintln count ;\n"
    )

    for i in range(1, no_of_test_cases):
        file.write(
            f"\tarr = {arrs[i]} ;\n\tcount = call count_{type}_arr({padding}{to_count[i]}{padding}, arr) ;\n\tprintln count ;\n"
        )


##### FUNCTION TO GENERATE FILL TESTS #####
def fill_arr(function_name, type, no_of_test_cases, file):
    arrs = list()
    out_arrs = list()
    to_fill_with = list()

    for i in range(no_of_test_cases):
        temp_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type[type]))
        arrs.append(temp_arr)
        to_fill_with.append(eval(rand_per_type[type]))
        out_arrs.append([to_fill_with[i] for j in range(i)])

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for i in range(no_of_test_cases):
        file.write(f"# arr_before = {arrs[i]}\n# arr_after = {out_arrs[i]}\n")

    padding = "'" if type == "char" else ""

    file.write(
        f'\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\tcall print_{type}_arr("arr_before", arr) ;\n\tcall {function_name}({padding}{to_fill_with[0]}{padding}, arr) ;\n\tcall print_{type}_arr("arr_after", arr) ;\n'
    )

    for i in range(1, no_of_test_cases):
        file.write(
            f'\tarr = {arrs[i]} ;\n\tcall print_{type}_arr("arr_before", arr) ;\n\tcall {function_name}({padding}{to_fill_with[i]}{padding}, arr) ;\n\tcall print_{type}_arr("arr_after", arr) ;\n'
        )


##### FUNCTION TO GENERATE FIND_NTH TESTS #####
def find_nth_arr(
    function_name,
    type,
    no_of_test_cases,
    file,
    nth_occurence=rand_per_type["positive_short_int"],
):
    arrs = list()
    to_find = list()
    index_of_appearance = list()

    for i in range(no_of_test_cases):
        temp_type = type if type == "bool" else "short_" + type
        temp_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type[temp_type]))
        arrs.append(temp_arr)
        # to_find is list of tuples of ('elem_to_search_for', 'nth_occurence')
        elem = eval(rand_per_type[temp_type])
        to_find.append((elem, eval(nth_occurence)))
        if to_find[i][1] > countOf(arrs[i], to_find[i][0]):
            index_of_appearance.append(-1)
        else:
            index_of_appearance.append(
                [ind for ind, int in enumerate(arrs[i]) if int == to_find[i][0]][
                    to_find[i][1] - 1
                ]
            )

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for i in range(no_of_test_cases):
        file.write(f"# {index_of_appearance[i]}\n")

    padding = "'" if type == "char" else ""

    file.write(
        f"\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\tint index = call find_nth_{type}_arr({padding}{to_find[0][0]}{padding}, {to_find[0][1]}, arr) ;\n\tprintln index ;\n"
    )

    for i in range(1, no_of_test_cases):
        file.write(
            f"\tarr = {arrs[i]} ;\n\tindex = call find_nth_{type}_arr({padding}{to_find[i][0]}{padding}, {to_find[i][1]}, arr) ;\n\tprintln index ;\n"
        )


##### FUNCTION TO GENERATE FIND_FIRST TESTS #####
def find_first_arr(function_name, type, no_of_test_cases, file, nth_occurence="1"):
    find_nth_arr(function_name, type, no_of_test_cases, file, nth_occurence)
    file.seek(0)
    to_write_back = file.read().replace("nth", "first").replace(", 1, arr", ", arr")
    file.seek(0)
    file.truncate()
    file.write(to_write_back)


##### FUNCTION TO GENERATE FIND_LAST TESTS #####
def find_last_arr(
    function_name,
    type,
    no_of_test_cases,
    file,
    nth_occurence="countOf(arrs[i], elem) if countOf(arrs[i], elem) != 0 else countOf(arrs[i], elem) + 1",
):
    find_nth_arr(function_name, type, no_of_test_cases, file, nth_occurence)
    file.seek(0)
    to_write_back = file.read().replace("nth", "last")
    to_write_back = sub(", \d+, arr", ", arr", to_write_back)
    file.seek(0)
    file.truncate()
    file.write(to_write_back)


##### FUNCTION TO GENERATE BINARY_SEARCH TESTS #####
def binary_search_arr(function_name, type, no_of_test_cases, file):
    arrs = list()
    to_target = list()

    for i in range(no_of_test_cases):
        temp_type = "short_" + type
        temp_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type[temp_type]))
        temp_arr.sort()
        arrs.append(temp_arr)
        to_target.append(eval(rand_per_type[temp_type]))

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for i in range(no_of_test_cases):
        file.write(f"# {binary_search(arrs[i], to_target[i])}\n")

    padding = "'" if type == "char" else ""

    file.write(
        f"\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\tint index = call binary_search_{type}_arr({padding}{to_target[0]}{padding}, arr) ;\n\tprintln index ;\n"
    )

    for i in range(1, no_of_test_cases):
        file.write(
            f"\tarr = {arrs[i]} ;\n\tindex = call binary_search_{type}_arr({padding}{to_target[i]}{padding}, arr) ;\n\tprintln index ;\n"
        )


##### FUNCTION TO GENERATE CONTAINS TESTS #####
def contains_arr(function_name, type, no_of_test_cases, file):
    arrs = list()
    contains = list()

    for i in range(no_of_test_cases):
        temp_type = type if type == "bool" else "short_" + type
        temp_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type[temp_type]))
        arrs.append(temp_arr)
        contains.append(eval(rand_per_type[temp_type]))

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for i in range(no_of_test_cases):
        file.write(f"# {contains[i] in arrs[i]}\n".lower().replace("'", ""))

    padding = "'" if type == "char" else ""

    file.write(
        f"\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\tbool contains = call contains_{type}_arr({padding}{contains[0]}{padding}, arr) ;\n\tprintln contains ;\n"
    )

    for i in range(1, no_of_test_cases):
        file.write(
            f"\tarr = {arrs[i]} ;\n\tcontains = call contains_{type}_arr({padding}{contains[i]}{padding}, arr) ;\n\tprintln contains ;\n"
        )


##### FUNCTION TO GENERATE COPY TESTS #####
def copy_arr(function_name, type, no_of_test_cases, file, clear=False):
    arrs = list()
    buff_arrs = list()
    reverse = list()
    no_of_test_cases = 10

    for i in range(no_of_test_cases):
        temp_arr = list()
        temp_buff_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type[type]))
            temp_buff_arr.append(eval(rand_per_type[type]))
        arrs.append(temp_arr)
        buff_arrs.append(temp_buff_arr)
        reverse.append(eval(rand_per_type["bool"]))

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for i in range(no_of_test_cases):
        file.write(
            f"# arr_before = {arrs[i]}\n# buff_before = {buff_arrs[i]}\n# arr_after = {list() if clear else arrs[i]}\n# buff_after = {arrs[i] if reverse[i] == 'false' else arrs[i][::-1]}\n"
        )

    single_quote = "'"

    if not clear:
        wacc_fun = function_name
    else:
        wacc_fun = f"move_{type}_arr"

    file.write(
        f'\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\t{type}[] buff_arr = {buff_arrs[0]} ;\n\tcall print_{type}_arr("arr_before", arr) ;\n\tcall print_{type}_arr("buff_before", buff_arr) ;\n\tcall {wacc_fun}(arr, buff_arr, {str(reverse[0]).lower().replace(single_quote, "")}) ;\n\tcall print_{type}_arr("arr_after", arr) ;\n\tcall print_{type}_arr("buff_after", buff_arr) ;\n'
    )

    for i in range(1, no_of_test_cases):
        file.write(
            f'\tarr = {arrs[i]} ;\n\tbuff_arr = {buff_arrs[i]} ;\n\tcall print_{type}_arr("arr_before", arr) ;\n\tcall print_{type}_arr("buff_before", buff_arr) ;\n\tcall {wacc_fun}(arr, buff_arr, {str(reverse[i]).lower().replace(single_quote, "")}) ;\n\tcall print_{type}_arr("arr_after", arr) ;\n\tcall print_{type}_arr("buff_after", buff_arr) ;\n'
        )


##### FUNCTION TO GENERATE MOVE TESTS #####
def move_arr(function_name, type, no_of_test_cases, file, clear=True):
    copy_arr(function_name, type, no_of_test_cases, file, clear)


##### FUNCTION TO GENERATE CONCATENATE TESTS #####
def concat_arrs(function_name, type, no_of_test_cases, file):
    arrs1 = list()
    arrs2 = list()
    concat_arrs = list()
    no_of_test_cases = 7

    for i in range(no_of_test_cases):
        temp_arr1 = list()
        temp_arr2 = list()
        for j in range(i):
            temp_arr1.append(eval(rand_per_type[type]))
            temp_arr2.append(eval(rand_per_type[type]))
        arrs1.append(temp_arr1)
        arrs2.append(temp_arr2)
        concat_arrs.append(
            [eval(rand_per_type[type])] * (len(temp_arr1) + len(temp_arr2))
        )

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for i in range(no_of_test_cases):
        file.write(
            f"# arr1 = {arrs1[i]}\n# arr2 = {arrs2[i]}\n# concat = {arrs1[i] + arrs2[i]}\n"
        )

    file.write(
        f'\n\nmod {path};\n\nbegin\n\t{type}[] arr1 = {arrs1[0]} ;\n\t{type}[] arr2 = {arrs2[0]} ;\n\t{type}[] concat = {concat_arrs[0]} ;\n\tcall {function_name}(arr1, arr2, concat) ;\n\tcall print_{type}_arr("arr1", arr1) ;\n\tcall print_{type}_arr("arr2", arr2) ;\n\tcall print_{type}_arr("concat", concat) ;\n'
    )

    for i in range(1, no_of_test_cases):
        file.write(
            f'\tarr1 = {arrs1[i]} ;\n\tarr2 = {arrs2[i]} ;\n\tconcat = {concat_arrs[i]} ;\n\tcall {function_name}(arr1, arr2, concat) ;\n\tcall print_{type}_arr("arr1", arr1) ;\n\tcall print_{type}_arr("arr2", arr2) ;\n\tcall print_{type}_arr("concat", concat) ;\n'
        )


##### FUNCTION TO GENERATE FOLD TESTS #####
def fold_arr(function_name, type, no_of_test_cases, file, fold_with_or = False):
    arrs = list()

    for i in range(no_of_test_cases):
        temp_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type["real_" + type]))
        arrs.append(temp_arr)

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    
    bool_fun = bool_or if fold_with_or else bool_and
    wacc_fun = f"fold_or_{type}_arr" if fold_with_or else f"fold_and_{type}_arr"

    file.write(f"# arr = {arrs[0]}\n# Given array to fold with {'OR' if fold_with_or else 'AND'} was empty!\n# false\n")

    for i in range(1, no_of_test_cases):
        file.write(f"# arr = {str(arrs[i]).lower()}\n# {str(reduce(bool_fun, arrs[i])).lower()}\n")

    file.write(
        f'\n\nmod {path};\n\nbegin\n\t{type}[] arr = {str(arrs[0]).lower()} ;\n\tcall print_{type}_arr("arr", arr) ;\n\tbool folded = call {wacc_fun}(arr) ;\n\tprintln folded ;\n'
    )

    for i in range(1, no_of_test_cases):
        file.write(f'\tarr = {str(arrs[i]).lower()} ;\n\tcall print_{type}_arr("arr", arr) ;\n\tfolded = call {wacc_fun}(arr) ;\n\tprintln folded ;\n')


##### FUNCTION TO GENERATE FOLD_AND TESTS #####
def fold_and_arr(function_name, type, no_of_test_cases, file, fold_with_or = False):
    fold_arr(function_name, type, no_of_test_cases, file, fold_with_or)

##### FUNCTION TO GENERATE FOLD_OR TESTS #####
def fold_or_arr(function_name, type, no_of_test_cases, file, fold_with_or = True):
    fold_arr(function_name, type, no_of_test_cases, file, fold_with_or)

##### DICTIONARY OF AVAILABLE FUNCTIONS TO GENERATE TESTS FOR AND WHAT TYPE OF TESTS CAN BE GENEREATED FOR THOSE FUNCTIONS #####
function_dict = {
    "print_arr": print_arr,
    "count_arr": count_arr,
    "fill_arr": fill_arr,
    "find_nth_arr": find_nth_arr,
    "find_first_arr": find_first_arr,
    "find_last_arr": find_last_arr,
    "binary_search_arr": binary_search_arr,
    "contains_arr": contains_arr,
    "copy_arr": copy_arr,
    "move_arr": move_arr,
    "concat_arrs": concat_arrs,
    "fold_and_arr": fold_and_arr,
    "fold_or_arr": fold_or_arr,
}
type_per_function = {
    "print_arr": ["int", "bool", "char"],
    "count_arr": ["int", "bool", "char"],
    "fill_arr": ["int", "bool", "char"],
    "find_nth_arr": ["int", "bool", "char"],
    "find_first_arr": ["int", "bool", "char"],
    "find_last_arr": ["int", "bool", "char"],
    "binary_search_arr": ["int", "char"],
    "contains_arr": ["int", "bool", "char"],
    "copy_arr": ["int", "bool", "char"],
    "move_arr": ["int", "bool", "char"],
    "concat_arrs": ["int", "bool", "char"],
    "fold_and_arr": ["bool"],
    "fold_or_arr": ["bool"],
}

##### MAIN FUNCTION #####
if __name__ == "__main__":
    test_gen_path = os.path.dirname(os.path.realpath(__file__))
    os.chdir(test_gen_path)

    n = int(input("How many functions would you like to test?\n\n"))

    print(
        "\nWhat test(s) and of what type(s) would you like?\nGive your choice(s) as 'function_with_type_placeholder type1,type2'\ne.g. 'print_type_arr int,bool' or 'print_type_arr all'\n"
    )

    tests_types = dict(input("").split() for _ in range(n))

    no_of_test_cases = 15

    for test in tests_types:
        type = tests_types[test]

        function_name = test.replace("_type_", "_")

        if function_name not in function_dict:
            exit(
                f"The given function ('{function_name}') wasn't found in the function dictionary!"
            )

        if not os.path.exists("generated_tests"):
            os.mkdir("generated_tests")

        os.chdir("generated_tests")

        if not os.path.exists(function_name):
            os.mkdir(function_name)

        os.chdir(function_name)

        if type == "all":
            selected_types = type_per_function[function_name]
        else:
            selected_types = type.split(",")

        for sel_type in selected_types:
            if sel_type not in type_per_function[function_name]:
                exit(f"Invalid type ('{sel_type}') given!")

            local_function_name = test.replace("_type_", f"_{sel_type}_")

            file = open(local_function_name + ".wacc", "w+")

            function_dict[function_name](
                local_function_name, sel_type, no_of_test_cases, file
            )

            file.seek(file.tell() - 3, os.SEEK_SET)
            file.write("\nend")

            if sel_type == "bool":
                file.seek(0)
                to_write_back = file.read().replace("'", "")
                file.seek(0)
                file.truncate()
                file.write(to_write_back)

            file.close()

        os.chdir("../..")
