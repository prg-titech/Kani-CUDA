package Expression;

import java.lang.Math;
import java.util.*;

public class LinearArithExpression {
	
	/*
	 * limit = [value of previous term, 
	 * 			value of previous unit,
	 * 			number of operations remaining,
	 * 			value of current term]
	 * 1 : constant		2 : variable
	 * 3 : +	4 : - 	5 : *
	 */
	
	public int[] arr;
	private int[] consts;
	private int[] vars;
	private int[][] profile;
	private int cSize;
	private int vSize;
	private int M;
	private int smidIndex;
	private int lineCount;
	private List<String> varNames;
	
	public LinearArithExpression(int[] consts, int[] vars,
			int[][] profile, int smidIndex, int lineCount, List<String> names) {
		this.consts = consts;
		this.vars = vars;
		this.profile = profile;
		this.smidIndex = smidIndex;
		this.lineCount = lineCount;
		this.varNames = names;
		cSize = consts.length;
		vSize = vars.length;
		M = cSize + vSize + 1;
	}
	
	public String generate_partial(int op_max, int max_error) {
		arr = new int[3 * op_max + 2];
		int[] limit = new int[4];
		limit[0] = Integer.MAX_VALUE;
		limit[1] = M - 2;
		limit[2] = op_max;
		limit[3] = 0;
		PartialCallBack tcb = new PartialCallBack(max_error);
		try {
			gen(0, limit, tcb);
		} catch (Exception e) {
			return e.getMessage();
		}
		return "f";
	}
	
	public String generate(int op_max) {
		arr = new int[3 * op_max + 2];
		int[] limit = new int[4];
		limit[0] = Integer.MAX_VALUE;
		limit[1] = M - 2;
		limit[2] = op_max;
		limit[3] = 0;
		TestCallBack tcb = new TestCallBack();
		try {
			gen(0, limit, tcb);
		} catch (Exception e) {
			return e.getMessage();
		}
		return "f";
	}
	
	public void gen(int index, int[] limit, CallBack cb) {
		gen_sum(index, limit, cb);
		//limit[0] = Integer.MAX_VALUE;
		//gen_minus(index, limit, cb);
	}
	
	public void gen_minus(int index, int[] limit, CallBack cb) {
		gen_term(index, limit, cb);
		if (limit[2] <= 0) { return; }
		gen_term(index, limit, new MinusCallBack(cb));
	}
	
	public void gen_sum(int index, int[] limit, CallBack cb) {
		//int[] lim_cpy = copy(limit);
		gen_term(index, limit, cb);
		if (limit[2] <= 0) { return; }
		gen_term(index, limit, new SumCallBack(cb));
	}
	
	public void gen_term(int index, int[] limit, CallBack cb) {
		//int[] lim_cpy = copy(limit);
		gen_unit(index, limit, cb);
		if (limit[2] <= 0) { return; }
		gen_unit(index, limit, new TermCallBack(cb));
	}
	
	public void gen_unit(int index, int[] limit, CallBack cb) {
		for (int i = 0; i <= Math.min(limit[1], cSize - 1); i++) {
			if (consts[i] == 1 && limit[3] != 0) { continue; }
			int[] lim_cpy = copy(limit);
			lim_cpy[3] = lim_cpy[3] * M + i + 1;
			if (lim_cpy[3] >= lim_cpy[0]) { return; }
			arr[index] = 1;
			arr[index + 1] = consts[i];
			lim_cpy[1] = i;
			cb.call(index + 2, lim_cpy, this);
		}
		
		for (int i = cSize; i <= limit[1]; i++) {
			int[] lim_cpy = copy(limit);
			lim_cpy[3] = lim_cpy[3] * M + i + 1;
			if (lim_cpy[3] >= lim_cpy[0]) { return; }
			arr[index] = 2;
			arr[index + 1] = vars[i - cSize];
			lim_cpy[1] = i;
			cb.call(index + 2, lim_cpy, this);
		}
	}
	
	public int[] getExpression() {
		return arr;
	}
	
	public int[] copy(int[] limit) {
		int[] lim_cpy = new int[limit.length];
		System.arraycopy( limit, 0, lim_cpy, 0, limit.length );
		return lim_cpy;
	}
	
	public int getM() {
		return M;
	}
	
	public void test(int end) {
		int x;
		for (int line = 0; line < lineCount; line++) {
			if ((x = profile[line][smidIndex]) != -1) {
				if (x != evaluate(end, line)) {
					return;
				}
			}
		}
		throw new RuntimeException(arithToString(end));
	}
	
	public void test_partial(int end, int max_error) {
		int x;
		int count = 0;
		for (int line = 0; line < lineCount; line++) {
			if ((x = profile[line][smidIndex]) != -1) {
				if (x != evaluate(end, line)) {
					count++;
				}
				if (count > max_error) { return; }
			}
		}
		throw new RuntimeException(arithToString(end));
	}
	
	public int evaluate(int end, int line) {
		Cursor cursor = new Cursor();
		int result = evalTerm(cursor, end, line);
		while(cursor.getIndex() < end) {
			switch (arr[cursor.getIndex()]) {
			case 3: cursor.addIndex(1);
					result += evalTerm(cursor, end, line);
					break;
			case 4: cursor.addIndex(1);
					result -= evalTerm(cursor, end, line);
					break;
			}
		}
		return result;
	}
	
	public int evalTerm(Cursor cursor, int end, int line) {
		int index = cursor.getIndex();
		//if (index >= end) { return 1; }
		int term = read(index, line);
		cursor.addIndex(2);
		if (cursor.getIndex() >= end) { return term; }
		while(cursor.getIndex() < end 
				&& arr[cursor.getIndex()] == 5) {
			cursor.addIndex(1);
			term *= evalTerm(cursor, end, line);
		}
		return term;
	}
	
	public int read(int index, int line) {
		if (arr[index] == 1) {
			return arr[index + 1];
		} else if (arr[index] == 2) {
			return profile[line][arr[index + 1]];
		}
		return 0;
	}
	
	public String arithToString(int end) {
		String result = " ";
		int index = 0;
		while (index < end) {
			switch(arr[index]) {
			case 1:
				result += arr[index + 1] + " ";
				index += 2;
				break;
			case 2:
				result += varNames.get(arr[index + 1]) + " ";
				index += 2;
				break;
			case 3:
				result += "+ ";
				index++;
				break;
			case 4:
				result += "- ";
				index++;
				break;
			case 5:
				result += "* ";
				index++;
				break;
			}
		}
		return result;
	}
	
}