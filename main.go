package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

type Event struct {
	EventCount, EventType int16
}

type Stack struct {
	StackCount int16
	StackType  string
}

type Wrong struct {
	WrongType        int8
	WrongDescription string
}

type Variable struct {
	VariableType, VariableValue string
}

const (
	STRING_ALPHABET                string = "^[a-z0-9\\.\"\\+\\-\\*/=!><\\s]$"
	STRING_VALID_INT               string = "^[0-9]+$"
	STRING_VALID_DOUBLE            string = "^[0-9]+\\.[0-9]+$"
	STRING_VALID_STRING            string = "^\".*\"$"
	STRING_VALID_ID                string = "^[a-z][a-z0-9]*$"
	STRING_ANYTHING                string = ".+"
	STRING_ARITHMETIC_OPERATOR     string = "\\+|-|\\*|/"
	STRING_LOGICAL_OPERATOR        string = "==|>|<"
	STRING_VARIABLE_TYPE           string = "int|double|string"
	STRING_ASSIGNMENT_OPERATION    string = "^(?P<value>" + STRING_ANYTHING + ")\\s(?P<operator>" + STRING_ARITHMETIC_OPERATOR + ")\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_IF_OPERATION            string = "^(?P<value>" + STRING_ANYTHING + ")\\s(?P<operator>" + STRING_LOGICAL_OPERATOR + ")\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_FOR_OPERATION           string = "^(?P<id>" + STRING_ANYTHING + ")\\s(?P<in>in)\\s(?P<value>" + STRING_ANYTHING + ")\\s(?P<points>\\.\\.\\.)\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_WHILE_OPERATION         string = "^(?P<value>" + STRING_ANYTHING + ")\\s(?P<operator>" + STRING_LOGICAL_OPERATOR + ")\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_MAIN_INSTRUCTION        string = "^(?P<instruction>main)$"
	STRING_DECLARATION_INSTRUCTION string = "^(?P<variable>" + STRING_VARIABLE_TYPE + ")\\s(?P<id>" + STRING_ANYTHING + ")$"
	STRING_ASSIGNMENT_INSTRUCTION  string = "^(?P<id>" + STRING_ANYTHING + ")\\s(?P<symbol>=)\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_PRINT_INSTRUCTION       string = "^(?P<instruction>print)\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_SCANNER_INSTRUCTION     string = "^(?P<instruction>scanner)\\s(?P<id>" + STRING_ANYTHING + ")$"
	STRING_IF_INSTRUCTION          string = "^(?P<instruction>if)\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_FOR_INSTRUCTION         string = "^(?P<instruction>for)\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_WHILE_INSTRUCTION       string = "^(?P<instruction>while)\\s(?P<value>" + STRING_ANYTHING + ")$"
	STRING_CLOSE_INSTRUCTION       string = "^(?P<instruction>close)\\s(?P<structure>" + STRING_ANYTHING + ")$"
)

var (
	alphabet       = regexp.MustCompile(STRING_ALPHABET)
	reserved_words = []string{"main", "int", "double", "string", "print", "scanner", "if", "for", "in", "while", "close"}
	wrong          = make(map[int16]Wrong)
	symbol         = make(map[string]Variable)
	semantic_stack []Stack
	event          []Event
	code_segment   []string
	data_segment   []string
	for_variable   string
	const_count    int16
	if_count       int16
	for_count      int16
	while_count    int16
	count          int16
)

func main() {
	input_file, err := os.Open("input file.txt")
	if err != nil {
		log.Fatal(err)
	}
	defer input_file.Close()
	program_file := bufio.NewScanner(input_file)
	lexical_analysis := func(value string) {
		var pass bool
		for _, char := range value {
			if string(char) == "\"" {
				pass = !pass
			}
			if !pass {
				if !alphabet.MatchString(string(char)) {
					event = append(event, Event{count + 1, 101})
				}
			}
		}
	}
	check_value := func(value string) (value_type string, value_accepted bool) {
		switch {
		case is_valid_int(value):
			value_type, value_accepted = "int", true
		case is_valid_double(value):
			value_type, value_accepted = "double", true
		case is_valid_string(value):
			value_type, value_accepted = "string", true
		case is_valid_id(value):
			switch {
			case is_reserved_word(value):
				event = append(event, Event{count + 1, 301})
			case !is_repeated_id(value):
				event = append(event, Event{count + 1, 303})
			default:
				value_type, value_accepted = symbol[value].VariableType, true
			}
		default:
			event = append(event, Event{count + 1, 203})
		}
		return
	}
	check_operation := func(variable_id string, value string, struct_cycle bool) (value_type string, value_accepted bool) {
		operation := regexp.MustCompile(STRING_ASSIGNMENT_OPERATION).FindSubmatch([]byte(value))
		if operation == nil {
			if !struct_cycle {
				code_segment = append(code_segment, "; asignación")
				if is_valid_int(string(value)) {
					code_segment = append(code_segment, fmt.Sprintf("mov %s, 3%vh", variable_id, value))
				} else if is_valid_id(string(value)) {
					// Actualización
					code_segment = append(code_segment, "push ax")
					code_segment = append(code_segment, fmt.Sprintf("mov ax, %v", value))
					code_segment = append(code_segment, fmt.Sprintf("mov %s, ax", variable_id))
					code_segment = append(code_segment, "pop ax")
				}
			}
			value_type, value_accepted = check_value(value)
		} else {
			var value_type_left, value_type_right string
			value_left, arithmetic_operator, value_right := string(operation[1]), string(operation[2]), string(operation[3])
			value_type_left, value_accepted_left := check_value(value_left)
			value_type_right, value_accepted_right := check_value(value_right)
			if value_accepted_left && value_accepted_right {
				if value_type_left == value_type_right {
					if value_type_left == "string" {
						if arithmetic_operator != "+" || arithmetic_operator != "-" || arithmetic_operator != "*" || arithmetic_operator != "/" {
							event = append(event, Event{count + 1, 308})
						}
					} else {
						value_type, value_accepted = value_type_left, true
					}
					if value_type_left == "int" {
						convert_number_to_assembler := func(register, value string) {
							code_segment = append(code_segment, "; asignación")
							if is_valid_int(value) {
								code_segment = append(code_segment, fmt.Sprintf("mov %sh, 0", register))
								code_segment = append(code_segment, fmt.Sprintf("mov %sl, 3%sh", register, value))
							} else {
								code_segment = append(code_segment, fmt.Sprintf("mov %sx, %s", register, value))
							}
						}
						convert_number_to_assembler("a", value_left)
						convert_number_to_assembler("b", value_right)
						if arithmetic_operator == "+" {
							code_segment = append(code_segment, "; suma")
							code_segment = append(code_segment, "add ax, bx")
							code_segment = append(code_segment, "sub ax, 30h")
						} else if arithmetic_operator == "-" {
							code_segment = append(code_segment, "; resta")
							code_segment = append(code_segment, "sub ax, bx")
							code_segment = append(code_segment, "add ax, 30h")
						} else if arithmetic_operator == "*" {
							code_segment = append(code_segment, "; multiplicación")
							code_segment = append(code_segment, "sub ax, 30h")
							code_segment = append(code_segment, "sub bx, 30h")
							code_segment = append(code_segment, "mul bl")
							code_segment = append(code_segment, "add ax, 30h")
						} else if arithmetic_operator == "/" {
							code_segment = append(code_segment, "; división")
							code_segment = append(code_segment, "sub ax, 30h")
							code_segment = append(code_segment, "sub bx, 30h")
							code_segment = append(code_segment, "div bl")
							code_segment = append(code_segment, "mov ah, 0")
							code_segment = append(code_segment, "add ax, 30h")
						} else {
							event = append(event, Event{count + 1, 308})
						}
						code_segment = append(code_segment, fmt.Sprintf("mov %s, ax", variable_id))
					}
				} else {
					event = append(event, Event{count + 1, 305})
				}
			}
		}
		return
	}
	for program_file.Scan() {
		text := program_file.Text()
		lexical_analysis(text)
		line, elements := must_compile([]byte(text))
		switch line {
		case "main":
			semantic_stack = append(semantic_stack, Stack{count + 1, "main"})
		case "declaration":
			variable_type, variable_id := string(elements[1]), string(elements[2])
			switch {
			case !is_valid_id(variable_id):
				event = append(event, Event{count + 1, 202})
			case is_reserved_word(variable_id):
				event = append(event, Event{count + 1, 301})
			case is_repeated_id(variable_id):
				event = append(event, Event{count + 1, 302})
			default:
				switch variable_type {
				case "int":
					data_segment = append(data_segment, "; variable int")
					data_segment = append(data_segment, (variable_id + " dw 30h, 10, 13, \"$\""))
					symbol[variable_id] = Variable{variable_type, "0"}
				case "double":
					symbol[variable_id] = Variable{variable_type, "0.0"}
				case "string":
					data_segment = append(data_segment, "; variable string")
					data_segment = append(data_segment, (variable_id + " db 255 DUP(\"$\")"))
					symbol[variable_id] = Variable{variable_type, "nil"}
				}
			}
		case "assignment":
			var variable_type string
			var variable_accepted bool
			variable_id, value := string(elements[1]), string(elements[3])
			switch {
			case !is_valid_id(variable_id):
				event = append(event, Event{count + 1, 202})
			case is_reserved_word(variable_id):
				event = append(event, Event{count + 1, 301})
			case !is_repeated_id(variable_id):
				event = append(event, Event{count + 1, 303})
			default:
				variable_accepted = true
				variable_type = symbol[variable_id].VariableType
			}
			if variable_accepted {
				value_type, value_accepted := check_operation(variable_id, value, false)
				if value_accepted {
					if variable_type != value_type {
						event = append(event, Event{count + 1, 304})
					}
				}
			}
		case "print":
			value := string(elements[2])
			code_segment = append(code_segment, "; print")
			code_segment = append(code_segment, "push ax")
			code_segment = append(code_segment, "push bx")
			switch {
			case is_valid_int(value) || is_valid_double(value) || is_valid_string(value):
				code_segment = append(code_segment, fmt.Sprintf("mov dx, offset const_%v", const_count))
				if is_valid_string(value) {
					data_segment = append(data_segment, fmt.Sprintf("const_%v db %s, 10, 13, \"$\"", const_count, value))
				} else {
					data_segment = append(data_segment, fmt.Sprintf("const_%v db \"%s\", 10, 13, \"$\"", const_count, value))
				}
				const_count++
			case !is_valid_id(value):
				event = append(event, Event{count + 1, 203})
			case is_reserved_word(value):
				event = append(event, Event{count + 1, 301})
			case !is_repeated_id(value):
				event = append(event, Event{count + 1, 303})
			default:
				code_segment = append(code_segment, ("mov dx, offset " + value))
			}
			code_segment = append(code_segment, "mov ah, 09h")
			code_segment = append(code_segment, "int 21h")
			code_segment = append(code_segment, "pop bx")
			code_segment = append(code_segment, "pop ax")
		case "scanner":
			variable_id := string(elements[2])
			switch {
			case !is_valid_id(variable_id):
				event = append(event, Event{count + 1, 202})
			case is_reserved_word(variable_id):
				event = append(event, Event{count + 1, 301})
			case !is_repeated_id(variable_id):
				event = append(event, Event{count + 1, 303})
			default:
				code_segment = append(code_segment, "; scanner")
				code_segment = append(code_segment, "mov ah, 01h")
				code_segment = append(code_segment, "int 21h")
				code_segment = append(code_segment, "mov ah, 0")
				code_segment = append(code_segment, ("mov " + variable_id + ", ax"))
			}
		case "if":
			operation := regexp.MustCompile(STRING_IF_OPERATION).FindSubmatch(elements[2])
			if operation == nil {
				event = append(event, Event{count + 1, 308})
			} else {
				var value_type_left, value_type_right string
				value_left, logical_operator, value_right := string(operation[1]), string(operation[2]), string(operation[3])
				value_type_left, value_accepted_left := check_value(value_left)
				value_type_right, value_accepted_right := check_value(value_right)
				if value_accepted_left && value_accepted_right {
					if value_type_left == value_type_right {
						code_segment = append(code_segment, "; if")
						if is_valid_id(value_left) {
							code_segment = append(code_segment, ("mov si, offset " + value_left))
						} else {
							code_segment = append(code_segment, fmt.Sprintf("mov si, offset 3%sh", value_left))
						}
						if is_valid_id(value_right) {
							code_segment = append(code_segment, ("mov di, offset " + value_right))
						} else {
							code_segment = append(code_segment, fmt.Sprintf("mov di, offset 3%sh", value_right))
						}
						code_segment = append(code_segment, "push ax")
						code_segment = append(code_segment, "mov ah, 0")
						code_segment = append(code_segment, "mov al, byte ptr [si]")
						code_segment = append(code_segment, "mov bh, 0")
						code_segment = append(code_segment, "mov bl, byte ptr [di]")
						code_segment = append(code_segment, "sub ax, bx")
						switch logical_operator {
						case "==":
							code_segment = append(code_segment, fmt.Sprintf("jne close_if_%v:", if_count))
						case ">":
							code_segment = append(code_segment, fmt.Sprintf("jna close_if_%v:", if_count))
						case "<":
							code_segment = append(code_segment, fmt.Sprintf("jnb close_if_%v:", if_count))
						}
					} else {
						event = append(event, Event{count + 1, 307})
					}
				}
			}
			semantic_stack = append(semantic_stack, Stack{count + 1, "if"})
		case "for":
			operation := regexp.MustCompile(STRING_FOR_OPERATION).FindSubmatch(elements[2])
			if operation == nil {
				event = append(event, Event{count + 1, 308})
			} else {
				var variable_type string
				var variable_accepted bool
				variable_id, value_left, value_right := string(operation[1]), string(operation[3]), string(operation[5])
				switch {
				case !is_valid_id(variable_id):
					event = append(event, Event{count + 1, 202})
				case is_reserved_word(variable_id):
					event = append(event, Event{count + 1, 301})
				case !is_repeated_id(variable_id):
					event = append(event, Event{count + 1, 303})
				default:
					variable_accepted = true
					variable_type = symbol[variable_id].VariableType
				}
				value_type_left, operation_accepted_left := check_operation(variable_id, value_left, true)
				value_type_right, operation_accepted_right := check_operation(variable_id, value_right, true)
				if variable_accepted && operation_accepted_left && operation_accepted_right {
					if value_type_left == value_type_right {
						if variable_type == value_type_left {
							number_left, _ := strconv.Atoi(value_left)
							number_right, _ := strconv.Atoi(value_right)
							code_segment = append(code_segment, "; for")
							code_segment = append(code_segment, fmt.Sprintf("mov %s, 3%vh", variable_id, value_left))
							code_segment = append(code_segment, fmt.Sprintf("mov cx, %v", (number_right-number_left+1)))
							code_segment = append(code_segment, fmt.Sprintf("for_%v:", for_count))
							for_variable = variable_id
						} else {
							event = append(event, Event{count + 1, 304})
						}
					} else {
						event = append(event, Event{count + 1, 305})
					}
				}
			}
			semantic_stack = append(semantic_stack, Stack{count + 1, "for"})
		case "while":
			operation := regexp.MustCompile(STRING_WHILE_OPERATION).FindSubmatch(elements[2])
			if operation == nil {
				event = append(event, Event{count + 1, 308})
			} else {
				var value_type_left, value_type_right string
				value_left, logical_operator, value_right := string(operation[1]), string(operation[2]), string(operation[3])
				value_type_left, value_accepted_left := check_value(value_left)
				value_type_right, value_accepted_right := check_value(value_right)
				if value_accepted_left && value_accepted_right {
					if value_type_left == value_type_right {
						code_segment = append(code_segment, "; while")
						code_segment = append(code_segment, "push ax")
						code_segment = append(code_segment, "push cx")
						code_segment = append(code_segment, fmt.Sprintf("while_%v:", while_count))
						if is_valid_id(value_right) {
							code_segment = append(code_segment, fmt.Sprintf("mov cx, %s", value_right))
						} else {
							code_segment = append(code_segment, "mov ch, 0")
							code_segment = append(code_segment, fmt.Sprintf("mov cl, 3%sh", value_right))
						}
						if is_valid_id(value_left) {
							code_segment = append(code_segment, fmt.Sprintf("cmp %s, cx", value_left))
						} else {
							code_segment = append(code_segment, "mov ah, 0")
							code_segment = append(code_segment, fmt.Sprintf("mov al, 3%sh", value_left))
							code_segment = append(code_segment, "cmp ax, cx")
						}
						switch logical_operator {
						case "==":
							code_segment = append(code_segment, fmt.Sprintf("jne close_while_%v:", while_count))
						case ">":
							code_segment = append(code_segment, fmt.Sprintf("jbe close_while_%v:", while_count))
						case "<":
							code_segment = append(code_segment, fmt.Sprintf("jae close_while_%v:", while_count))
						}
					} else {
						event = append(event, Event{count + 1, 307})
					}
				}
			}
			semantic_stack = append(semantic_stack, Stack{count + 1, "while"})
		case "close":
			close_type := string(elements[2])
			if close_type == "main" || close_type == "if" || close_type == "for" || close_type == "while" {
				switch close_type {
				case "if":
					code_segment = append(code_segment, fmt.Sprintf("close_if_%v:", if_count))
					code_segment = append(code_segment, "pop ax")
					if_count++
				case "for":
					code_segment = append(code_segment, fmt.Sprintf("inc %s", for_variable))
					code_segment = append(code_segment, fmt.Sprintf("loop for_%v:", for_count))
					for_count++
				case "while":
					code_segment = append(code_segment, fmt.Sprintf("jmp while_%v:", while_count))
					code_segment = append(code_segment, fmt.Sprintf("close_while_%v:", while_count))
					code_segment = append(code_segment, "pop cx")
					code_segment = append(code_segment, "pop ax")
					while_count++
				}
				if semantic_stack == nil {
					event = append(event, Event{count + 1, 309})
				} else {
					var stack_type_found bool
					for i := len(semantic_stack); i > 0 && !stack_type_found; i-- {
						if close_type == semantic_stack[i-1].StackType {
							stack_type_found = true
							semantic_stack = append(semantic_stack[:i-1], semantic_stack[i:]...)
						}
					}
					if !stack_type_found {
						event = append(event, Event{count + 1, 309})
					}
				}
			} else {
				event = append(event, Event{count + 1, 308})
			}
		default:
			event = append(event, Event{count + 1, 201})
		}
		fmt.Printf("\nLínea %d: %s\n", (count + 1), text)
		count++
	}
	wrong[101] = Wrong{1, "El carácter no pertenece al alfabeto del lenguaje."}
	wrong[201] = Wrong{2, "La instrucción es incorrecta."}
	wrong[202] = Wrong{2, "El identificador es incorrecto."}
	wrong[203] = Wrong{2, "El valor asignado es incorrecto."}
	wrong[301] = Wrong{3, "La variable está usando como identificador una palabra reservada."}
	wrong[302] = Wrong{3, "La variable está usando un identificador repetido."}
	wrong[303] = Wrong{3, "La variable no está declarada."}
	wrong[304] = Wrong{3, "La variable no soporta el valor asignado."}
	wrong[305] = Wrong{3, "La operación aritmética está usando tipo de datos distintos."}
	wrong[306] = Wrong{3, "El tipo de dato de la variable no es soportado por la instrucción."}
	wrong[307] = Wrong{3, "La operación lógica está usando tipo de datos distintos."}
	wrong[308] = Wrong{3, "La expresión de la estructura es incorrecta."}
	wrong[309] = Wrong{3, "La instrucción close no corresponde a la apertura de una estructura de su tipo."}
	wrong[310] = Wrong{3, "La estructura no corresponde a la clausura de una instrucción close de su tipo."}
	for _, struct_stack := range semantic_stack {
		event = append(event, Event{struct_stack.StackCount, 310})
	}
	if event == nil {
		output_file, err := os.Create("output file.asm")
		if err != nil {
			log.Fatal(err)
		}
		output_file.Write([]byte(".model small\n"))
		output_file.Write([]byte(".code\n"))
		output_file.Write([]byte("main:\n"))
		output_file.Write([]byte("mov ax, @data\n"))
		output_file.Write([]byte("mov ds, ax\n"))
		for i := range code_segment {
			output_file.Write([]byte(code_segment[i] + "\n"))
		}
		output_file.Write([]byte("mov ax, 4c00h\n"))
		output_file.Write([]byte("int 21h\n"))
		output_file.Write([]byte(".data\n"))
		for i := range data_segment {
			output_file.Write([]byte(data_segment[i] + "\n"))
		}
		output_file.Write([]byte(".stack\n"))
		output_file.Write([]byte("end main"))
		defer output_file.Close()
	} else {
		fmt.Println("\nTabla de eventos")
		fmt.Printf("%s\t%s\t%s\n", "Línea", "Tipo de error", "Descripción")
		for _, struct_event := range event {
			var event_type string
			struct_wrong := wrong[struct_event.EventType]
			switch struct_wrong.WrongType {
			case 1:
				event_type = fmt.Sprintf("%d\t%s\t\t%s", struct_event.EventCount, "Léxico", struct_wrong.WrongDescription)
			case 2:
				event_type = fmt.Sprintf("%d\t%s\t%s", struct_event.EventCount, "Sintáctico", struct_wrong.WrongDescription)
			case 3:
				event_type = fmt.Sprintf("%d\t%s\t%s", struct_event.EventCount, "Semántico", struct_wrong.WrongDescription)
			}
			fmt.Println(event_type)
		}
	}
}

func must_compile(program_line []byte) (description string, instruction [][]byte) {
	main_instruction := regexp.MustCompile(STRING_MAIN_INSTRUCTION).FindSubmatch(program_line)
	declaration_instruction := regexp.MustCompile(STRING_DECLARATION_INSTRUCTION).FindSubmatch(program_line)
	assignment_instruction := regexp.MustCompile(STRING_ASSIGNMENT_INSTRUCTION).FindSubmatch(program_line)
	print_instruction := regexp.MustCompile(STRING_PRINT_INSTRUCTION).FindSubmatch(program_line)
	scanner_instruction := regexp.MustCompile(STRING_SCANNER_INSTRUCTION).FindSubmatch(program_line)
	if_instruction := regexp.MustCompile(STRING_IF_INSTRUCTION).FindSubmatch(program_line)
	for_instruction := regexp.MustCompile(STRING_FOR_INSTRUCTION).FindSubmatch(program_line)
	while_instruction := regexp.MustCompile(STRING_WHILE_INSTRUCTION).FindSubmatch(program_line)
	close_instruction := regexp.MustCompile(STRING_CLOSE_INSTRUCTION).FindSubmatch(program_line)
	switch {
	case main_instruction != nil:
		description, instruction = "main", main_instruction
	case declaration_instruction != nil:
		description, instruction = "declaration", declaration_instruction
	case assignment_instruction != nil:
		description, instruction = "assignment", assignment_instruction
	case print_instruction != nil:
		description, instruction = "print", print_instruction
	case scanner_instruction != nil:
		description, instruction = "scanner", scanner_instruction
	case if_instruction != nil:
		description, instruction = "if", if_instruction
	case for_instruction != nil:
		description, instruction = "for", for_instruction
	case while_instruction != nil:
		description, instruction = "while", while_instruction
	case close_instruction != nil:
		description, instruction = "close", close_instruction
	}
	return
}

func is_valid_int(variable_id string) bool {
	return regexp.MustCompile(STRING_VALID_INT).MatchString(variable_id)
}

func is_valid_double(variable_id string) bool {
	return regexp.MustCompile(STRING_VALID_DOUBLE).MatchString(variable_id)
}

func is_valid_string(variable_id string) bool {
	return regexp.MustCompile(STRING_VALID_STRING).MatchString(variable_id)
}

func is_valid_id(variable_id string) bool {
	return regexp.MustCompile(STRING_VALID_ID).MatchString(variable_id)
}

func is_repeated_id(variable_id string) bool {
	_, exists := symbol[variable_id]
	return exists
}

func is_reserved_word(variable_id string) bool {
	for _, reserved_word := range reserved_words {
		if reserved_word == variable_id {
			return true
		}
	}
	return false
}
