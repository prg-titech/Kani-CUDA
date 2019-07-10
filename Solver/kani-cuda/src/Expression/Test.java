package Expression;

import java.util.*;

public class Test {
	
	//static int[] arr = new int[] {1, 2, 5, 1, 3};
	
	public static void main(String[] args) {
		
		
		//LinearArithExpression exp = new LinearArithExpression(
		//		new int[] {1, 2}, new int[] { -1, -2, -3, -4, -5 }) ;
		//exp.generate(3);
		
		//System.out.println(evaluate(5));
		
		LinearLogicExpression bexp = new LinearLogicExpression(
				new int[] {1}, new int[] { -1, -2, -3, -4, -5, -6, -7, -8}) ;
		//bexp.generate();
	}
	
	/*
	public static int evaluate(int end) {
		Cursor cursor = new Cursor();
		int result = evalTerm(cursor, end);
		while(cursor.getIndex() < end) {
			switch (arr[cursor.getIndex()]) {
			case 3: cursor.addIndex(1);
					result += evalTerm(cursor, end);
					break;
			case 4: cursor.addIndex(1);
					result -= evalTerm(cursor, end);
					break;
			}
		}
		return result;
	}
	
	public static int evalTerm(Cursor cursor, int end) {
		int index = cursor.getIndex();
		int term = read(index);
		cursor.addIndex(2);
		if (cursor.getIndex() >= end) { return term; }
		while(cursor.getIndex() < end && arr[cursor.getIndex()] == 5) {
			cursor.addIndex(1);
			term *= evalTerm(cursor, end);
		}
		return term;
	}
	
	public static int read(int index) {
		if (arr[index] == 1) {
			return arr[index + 1];
		} else if (arr[index] == 2) {
			//TODO: this should be the variable value
			return arr[index + 1];
		}
		return 0;
	}
	*/
}