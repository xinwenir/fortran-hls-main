import re

modified_ir = "define void @_Z7pipe_test_(ptr %a) store i32 1, ptr %1, align 4"

first_sub_result = re.sub(r'define\s+void\s+@([a-zA-Z_][a-zA-Z0-9_]*)\(ptr\s+%([a-zA-Z_][a-zA-Z0-9_]*)\)', r'define void @\1(i8* %\2)', modified_ir)
print("first_sub_result:", first_sub_result)

second_sub_result = re.sub(r'\bptr\s+([a-zA-Z_][a-zA-Z0-9_]*)\b', r'i8* %\1', first_sub_result)
print("second_sub_result:", second_sub_result)

parts = second_sub_result.split(" ")
for i, part in enumerate(parts):
    if part.startswith("ptr") and len(part) > 3:
        parts[i] = "i8* " + part[4:]
modified_ir = " ".join(parts)
print("modified_ir:", modified_ir)
modified_string = re.sub(r'\bptr\b', 'i8*', modified_ir)
print("modified_string:", modified_string)