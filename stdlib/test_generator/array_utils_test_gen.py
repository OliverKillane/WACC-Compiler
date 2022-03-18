from operator import countOf
import os
from sys import exit
from random import getrandbits, randint, choice

##### DATA TO CHOOSE FROM DEPENDING ON THE TYPE #####
rand_per_type = {
    "int": "randint(-2147483648, 2147483647)",
    "bool": "str(bool(getrandbits(1))).lower()",
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

    path = "../../../../../stdlib/array_utils.wacc"
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

    path = "../../../../../stdlib/array_utils.wacc"

    padding = "'" if type == "char" else ""

    file.write(
        f"\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\tint count = call count_{type}_arr({padding}{to_count[0]}{padding}, arr) ;\n\tprintln count ;\n"
    )

    for i in range(1, no_of_test_cases):
        file.write(
            f"\tarr = {arrs[i]} ;\n\tcount = call count_{type}_arr({padding}{to_count[i]}{padding}, arr) ;\n\tprintln count ;\n"
        )


##### FUNCTION TO GENERATE FILL TESTS #####
def fill_arr(function_name, type, no_of_testcases, file):
    arrs = list()
    out_arrs = list()
    to_fill_with = list()

    for i in range(no_of_testcases):
        temp_arr = list()
        for j in range(i):
            temp_arr.append(eval(rand_per_type[type]))
        arrs.append(temp_arr)
        to_fill_with.append(eval(rand_per_type[type]))
        out_arrs.append([to_fill_with[i] for j in range(i)])

    file.write(f"# Correctness of {function_name}()\n\n")
    file.write("# Output:\n")

    for i in range(no_of_testcases):
        file.write(f"# arr_before = {arrs[i]}\n# arr_after = {out_arrs[i]}\n")

    path = "../../../../../stdlib/array_utils.wacc"

    padding = "'" if type == "char" else ""

    file.write(
        f'\n\nmod {path};\n\nbegin\n\t{type}[] arr = {arrs[0]} ;\n\tcall print_{type}_arr("arr_before", arr) ;\n\tcall {function_name}({padding}{to_fill_with[0]}{padding}, arr) ;\n\tcall print_{type}_arr("arr_after", arr) ;\n'
    )

    for i in range(1, no_of_testcases):
        file.write(
            f'\tarr = {arrs[i]} ;\n\tcall print_{type}_arr("arr_before", arr) ;\n\tcall {function_name}({padding}{to_fill_with[i]}{padding}, arr) ;\n\tcall print_{type}_arr("arr_after", arr) ;\n'
        )


##### DICTIONARY OF AVAILABLE FUNCTIONS TO GENERATE TESTS FOR AND WHAT TYPE OF TESTS CAN BE GENEREATED FOR THOSE FUNCTIONS #####
function_dict = {"print_arr": print_arr, "count_arr": count_arr, "fill_arr": fill_arr}
type_per_function = {
    "print_arr": ["int", "bool", "char"],
    "count_arr": ["int", "bool", "char"],
    "fill_arr": ["int", "bool", "char"],
}

##### MAIN FUNCTION #####
if __name__ == "__main__":

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
