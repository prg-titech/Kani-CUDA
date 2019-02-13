package Expression;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.List;

import org.junit.Test;

public class DeepCopyExamples {
	
	@Test
	public void test() {
		Expression x = new Variable("x");
		Expression y = new Variable("y");
		Expression z = new Variable("z");
		List<Expression> lst = new ArrayList<Expression>();
		lst.add(x);
		lst.add(y);
		List<Expression> lstcp = new ArrayList(lst);
		lstcp.set(0, z);
		System.out.println("lst");
		for(Expression e : lst){
			e.print();
		}
		System.out.println("lstcp");
		for(Expression e : lstcp){
			e.print();
		}
		
	}

}
