custom = open("test.txt")
correct = open("/Users/siddharth/nes_ebook/code/ch3.4/test.txt")

custom_lines = custom.readlines()
correct_lines = correct.readlines()

issue_found = False

for i in range(len(custom_lines)):
    custom_parts = custom_lines[i].split()
    custom_opcode = custom_parts[2]
    custom_x = custom_parts[4]
    custom_y = custom_parts[5]
    custom_a = custom_parts[6]

    correct_parts = correct_lines[i].split()
    correct_opcode = correct_parts[2]
    correct_x = correct_parts[4]
    correct_y = correct_parts[5]
    correct_a = correct_parts[6]

    if (custom_opcode != correct_opcode or custom_x != correct_x or custom_y != correct_y or custom_a != correct_a):
        issue_found = True

        print("Issue on line: " , i)

        print("Custom:")
        print(custom_lines[i - 2].strip())
        print(custom_lines[i - 1].strip())
        print(custom_lines[i].strip())

        print("\nCorrect:")
        print(correct_lines[i - 2].strip())
        print(correct_lines[i - 1].strip())
        print(correct_lines[i].strip())
        break


if issue_found == False:
    print("No issues found")