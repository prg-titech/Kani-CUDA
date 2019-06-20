package Expression;

public interface BoolExpression {
	BoolExpression unOp(String op);
	
	BoolExpression binOp(String op, BoolExpression that);
	
	void print();
	
	String toStringExp();
	
	boolean eval(Environment env);
}
