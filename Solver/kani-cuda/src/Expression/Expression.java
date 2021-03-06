package Expression;

public interface Expression {
	// Add expressions
	Expression add(Expression that);
	
	// Subtract expressions
	Expression subtract(Expression that);
	
	// Multiple expressions
	Expression multiple(Expression that);

	// Bin expressions
	BoolExpression binOp(String op, Expression that);
	
	// Boolean value as to this expression is a variable
	boolean isVariable();
	
	// Boolean value as to this expression is a constant
	boolean isConsatnt();
	
	// Evaluate this expression in env
	int eval(Environment env);
	
	// Convert this expression to String
	String toStringExp();
	
	// Print this expression
	void print();
	
	// Get order of this expression
	int getOrder();	
	
	int contain(int num, Expression that);
	
	boolean isReverse(Expression that);
}
