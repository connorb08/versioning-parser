# Connor Bray
# Language Lexer and Parser
# 4/8/2022

import ply.lex as lex
import ply.yacc as yacc
import argparse

class ClifLexer():

	# CONSTRUCTOR
	def __init__(self):
		self.lexer = lex.lex(module=self)
		# start in the (standard) initial state
		self.lexer.begin('INITIAL')

	# DESTRUCTOR
	def __del__(self):
		pass

	reserved_bool = {
		'and': 'AND',
		'or': 'OR',
		'iff' : 'IFF',
		'if' : 'IF',
		'not' : 'NOT',
	}

	tokens = ['OPEN', 'CLOSE', 'INNERSTRINGQUOTE', 'RESERVEDELEMENT', 'NUMERAL', 'DIGIT', 'STRINGQUOTE', 'NAMEQUOTE', 'CHAR', 'COMMENT']

	tokens += reserved_bool.values()

	states = (
		("COMM", "exclusive"),
	)

	t_ignore = ' \t\r\n\f\v'
	t_COMM_ignore = ' \t\r\n\f\v'

	def t_NEWLINE(self,t):
		r'\n+'
		t.lexer.lineno += len(t.value)

	def t_error(self,t):
		print("Lexing error: Unknown character \"{}\" at line {}".format(t.value[0], t.lexer.lineno))
		t.lexer.skip(1)

	# token specification as a string (no regular expression)
	t_OPEN= '\('
	t_CLOSE= '\)'
	t_STRINGQUOTE = '\''
	t_NAMEQUOTE = '\"'

	def t_RESERVEDELEMENT(self, t):
		r'and|or|iff|if|not'
		if t.value in self.reserved_bool:
			t.type = self.reserved_bool[t.value]
			return t
		else:
			pass

	def t_DIGIT(self, t):
		r"[0-9]"
		return t

	def t_NUMERAL(self, t):
		r"[0-9]+"
		return t

	def t_CHAR(self, t):
		r"[\w~!#$%^&*+{}|:<>?\-\=[\];,./]"
		return t
	
	def t_INNERSTRINGQUOTE(self, t):
		r"\\'"
		return t

	#COMMENT STATE
	# Ignore everything inside a comment

	def t_COMMENT(self, t):
		r'\(cl:comment'
		self.lexer.level = 1
		self.lexer.begin("COMM")
		return t
	
	def t_COMM_lpar(self, t):
		r"\("
		self.lexer.level += 1

	def t_COMM_rpar(self, t):
		r"\)"
		self.lexer.level -= 1

		if t.lexer.level == 0: # break out of comment if matching parenthesis
			self.lexer.begin("INITIAL")
	
	def t_COMM_body(self, t):
		r"."
		pass

	def t_COMM_error(self, t):
		r"."
		print("Comment error:", t.value)

	def lex(self, input_string):
		self.lexer.input(input_string)
		while True:
			tok = self.lexer.token()
			if not tok:
				break
			print(str(tok).strip())

class ClifParser(object):

	tokens = ClifLexer.tokens

	# CONSTRUCTOR
	def __init__(self):
		self.lexer = ClifLexer()
		self.parser = yacc.yacc(module=self)
		self.operations = 0
		self.current_name = ""
		self.names = []
		self.sentences = 0
		self.bool_sent = 0
		self.atom_sent = 0
		self.comment_sent = 0
		self.error = 0
	
	# Parser destructor
	def __del__(self):
		pass

	def p_starter(self, p):
		"""
		starter : sentence
				| sentence starter
		"""
		print(f"Found a starter: {p.slice[1:]}")
		self.sentences += 1
		pass

	def p_char(self, p):
		"""
		char : CHAR
				| DIGIT
		"""
		print(f"Found a char: {p.slice[1:]}")

	def p_quotedstring(self, p):
		"""
		quotedstring : STRINGQUOTE innerquotedstring STRINGQUOTE 
		"""
		print(f"Found a quotedstring: {p.slice[1:]}")
		if not self.current_name in self.names: # only add name to list if it is unique
			self.names.append(self.current_name)
			self.current_name = "" # reset current name
	
	def p_innerquotedstring(self, p):
		"""
		innerquotedstring :	CHAR
							| AND
							| OR
							| IF
							| IFF
							| NOT
							| DIGIT
							| NAMEQUOTE
							| INNERSTRINGQUOTE
							| COMMENT
							| innerquotedstring CHAR
							| innerquotedstring AND
							| innerquotedstring OR
							| innerquotedstring IF
							| innerquotedstring IFF
							| innerquotedstring NOT
							| innerquotedstring DIGIT
							| innerquotedstring NAMEQUOTE
							| innerquotedstring INNERSTRINGQUOTE
							| innerquotedstring COMMENT
		"""
		print(f"Found an innerquotedstring: {p.slice[1:]}")
		if p[1]:
			self.current_name += p[1]
		elif p[2]:
			self.current_name += p[2]

	def p_numeral(self, p):
		"""
		numeral : DIGIT 
				| numeral DIGIT
		"""
		print(f"Found a numeral: {p.slice[1:]}")

	def p_interpretedname(self, p):
		"""
		interpretedname : numeral
						| quotedstring
		"""
		print(f"Found an interpretedname: {p.slice[1:]}")
	
	def p_predicate(self, p):
		"""
		predicate : interpretedname
		"""
		print(f"Found a predicate: {p.slice[1:]}")
	
	def p_termseq(self, p):
		"""
		termseq : interpretedname
				| termseq interpretedname
		"""
		print(f"Found a termseq: {p.slice[1:]}")

	def p_sentence(self, p):
		"""
		sentence : atomsent
				 | boolsent
				 | commentsent
		"""
		print(f"Found a sentence: {p.slice[1:]}")

	def p_sentences(self, p):
		"""
		sentences : sentence
					| sentences sentence
		"""
		print(f"Found a sentences: {p.slice[1:]}")

	def p_atomsent(self, p):
		"""
		atomsent : OPEN predicate CLOSE
					| OPEN termseq CLOSE
					| OPEN predicate termseq CLOSE
					| OPEN predicate sentence CLOSE
					| OPEN predicate sentences CLOSE
		"""
		self.atom_sent = 1
		self.comment_sent = 0
		print(f"Found an atomic sent: {p.slice[1:]}")
	
	def p_boolsent(self, p):
		"""
		boolsent : OPEN AND sentences CLOSE
				 | OPEN OR sentences CLOSE
				 | OPEN AND termseq CLOSE
				 | OPEN OR termseq CLOSE
				 | OPEN IF sentence sentence CLOSE
				 | OPEN IFF sentence sentence CLOSE
				 | OPEN NOT sentence CLOSE
				 | OPEN NOT termseq CLOSE
		"""
		self.bool_sent = 1
		self.atom_sent = 0
		self.comment_sent = 0
		self.operations += 1
		print(f"Found a bool sentence: {p.slice[1:]}")
	
	def p_commentsent(self, p):
		"""
		commentsent : COMMENT
		"""
		self.comment_sent = 1
		self.bool_sent = 0
		self.atom_sent = 0
		print(f"Found a comment sentence: {p.slice[1:]}")

	def p_error(self, p):

		self.error = 1

		if p is None:
			print("Unexpectedly reached end of file (EOF)")
			return
			#raise TypeError("Unexpectedly reached end of file (EOF)")

		# Note the location of the error before trying to lookahead
		error_pos = p.lexpos

		# Reading the symbols from the Parser stack
		stack = [symbol for symbol in self.parser.symstack][1:]

		print("Parsing error; current stack: " + str(stack))


	def parse(self, input_string):

		self.parser.parse(input_string)

		if self.error:
			return f"Error parsing {input_string.rstrip()}\n"
		elif self.bool_sent:
			return f"Boolean: {input_string.rstrip()}: ops={self.operations} names={len(self.names)}\n"
		elif self.comment_sent:
			self.sentences -= 1 # dont count if sentence is a comment
			return f"comment: {input_string.rstrip()}: ops={self.operations} names={len(self.names)}\n"
		elif self.atom_sent:
			return f"atomic: {input_string.rstrip()}: ops={self.operations} names={len(self.names)}\n"
		else:
			return f"Error: input_string:{input_string}"

def main(rel_clif_path, run_parser):

	parse_string = ""
	sentences = 0

	lex = ClifLexer()
	myFile = open(rel_clif_path)

	with myFile as fp:
		for line in fp:
			try:

				if run_parser:
					parser = ClifParser()
					print("\n")
					print(f"Parsing {line.strip()}")
					parse_string += parser.parse(line)
					sentences += parser.sentences

				else: 
					print(f"Lexing {line.strip()}")
					lex.lex(line)
					print("\n")
				
			except EOFError:
				break

	myFile.close()

	if run_parser:
		print(f"\n{sentences} sentences")
		print(parse_string)


if __name__ == '__main__':

	arg_parser = argparse.ArgumentParser(description="Description")
	# Relative path to CLIF file
	arg_parser.add_argument("rel_clif_path", metavar="x", type=str, help="Relative path to CLIF file")
	# Boolean if the program should run parser
	arg_parser.add_argument("run_parser", metavar="y", type=str, help="Boolean for running parser")

	program_args = vars(arg_parser.parse_args())

	rel_clif_path = program_args["rel_clif_path"]
	run_parser = program_args["run_parser"]

	main(rel_clif_path, run_parser.lower() != 'false')