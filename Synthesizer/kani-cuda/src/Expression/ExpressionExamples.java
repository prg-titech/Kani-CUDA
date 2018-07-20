package Expression;

import static org.junit.Assert.*;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.junit.Test;

public class ExpressionExamples {
	Expression one = new Constant<Integer>(1);
	Expression varX = new Variable("x");
	Expression varY = new Variable("y");	
	Environment env = new Environment();
	
	// x + y + 1
	Expression arith1 = varX.add(varY).add(one);
	
	// x - (y - 1)
	Expression arith2 = varX.subtract(varY.subtract(one));
	
	// x * x * y
	Expression arith3 = varY.multiple(varX.multiple(varX));
	
	// x == y - 1
	Expression bool1 = varX.binOp("==", varY.subtract(one));
	
	// !(x == 1) && (x == y - 1)
	Expression bool2 = varX.binOp("==" , one).unOp("!").binOp("&&", bool1);

	@Test
	public void testArith() {
		env.putEnv("x", 2);
		env.putEnv("y", 3);
		one.print();

		varX.print();
		arith1.print();
		arith2.print();
		assertEquals(1, one.eval(env));
		assertEquals(2, varX.eval(env));
		assertEquals(6, arith1.eval(env));
		assertEquals(0, arith2.eval(env));
		assertEquals(12, arith3.eval(env));
	}
	
	@Test
	public void testBool() {
		System.out.println(bool1.toStringExp());
		bool2.print();
	}
	
	//@Test
	public void testGenerateExp() {
		env.putEnv("x", 1);
		env.putEnv("y", 0);
		//env.addVar("z", 0);
		//env.addVar("w", 0);
		
		// Generate arbitrary expression of depth 3
		List<Expression> lst = env.generateArith(3);
		int size = lst.size();
		for(int i = 0; i < size; i++){
			lst.get(i).print();
		}
	}
	
	//@Test
	public void testProfile() {
		try{
			File file = new File("/Users/akira/masuhara-lab/kani-cuda/Kani-CUDA/Examples/Diffusion3d/profile");
			
			if(file.exists()){
				FileReader fr = new FileReader(file);
				BufferedReader br = new BufferedReader(fr);
				String datum = br.readLine();
				
				// Save the names of variables
				List<String> vars = Arrays.asList(datum.split(" "));
				System.out.println(vars.size());
				
				List<String> data = new ArrayList<String>();
				while ((datum = br.readLine()) != null) {
					data.add(datum);
				}
				fr.close();
				
				for (String d : data) {
					System.out.println(d);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	
	@Test
	public void testOrder() {
		assertEquals(1, arith1.getOrder());
		assertEquals(1, arith2.getOrder());
		assertEquals(3, arith3.getOrder());
	}

}