custom = open("vram.txt")
correct = open("/Users/siddharth/nes_ebook/code/ch7/vram.txt")

custom_lines = custom.readlines()
correct_lines = correct.readlines()

issue_found = False

for i in range(len(custom_lines)):
    try:
        custom_parts = custom_lines[i].split()
        custom_addr = custom_parts[1]
        custom_value = custom_parts[3]

        correct_parts = correct_lines[i].split()
        correct_addr = correct_parts[1]
        correct_value = correct_parts[3]

        if (custom_addr != correct_addr or custom_value != correct_value):
            issue_found = True

            print("Issue on line: " , i)

            print("Custom:")
            print(custom_lines[i].strip())

            print("\nCorrect:")
            print(correct_lines[i].strip())
            break
    except:
        print("Error: Reached end of file ", + i)
        exit(0)

if issue_found == False:
    print("No issues found")