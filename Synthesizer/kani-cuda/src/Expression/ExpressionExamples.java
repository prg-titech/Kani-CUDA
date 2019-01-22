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
	
	// x * y * x
	Expression arith4 = varX.multiple(varY.multiple(varX));
	
	// x * x * y
	Expression arith5 = varY.multiple(varX.multiple(varX));
	
	// x == y - 1
	BoolExpression bool1 = varX.binOp("==", varY.subtract(one));
	
	// !(x == 1) && (x == y - 1)
	BoolExpression bool2 = varX.binOp("==" , one).unOp("!").binOp("&&", bool1);
	
	// x > y
	BoolExpression bool3 = varX.binOp(">", varY);
	
	// if (x > y) {x} else {y}
	IfExpression ifexp = new IfExpression(bool3, varX, varY);
	

	//@Test
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
	
	//@Test
	public void testBool() {
		env.putEnv("x", 2);
		env.putEnv("y", 3);
		
		//System.out.println(bool1.eval(env));
		
		assertEquals(true, bool1.eval(env));
		assertEquals(true, bool2.eval(env));
	}
	
	//@Test
	public void testIf(){
		env.putEnv("x", 2);
		env.putEnv("y", 3);
		
		ifexp.print();
		assertEquals(3 , ifexp.eval(env)); 
	}
	
	//@Test
	public void testGenerateExp() {
		env.putEnv("a", 1);
		env.putEnv("b", 0);
		env.putEnv("c", 0);
		env.putEnv("d", 0);
		env.putEnv("e", 0);
		//env.putEnv("f", 0);
		//env.addVar("z", 0);
		//env.addVar("w", 0);
		
		// Generate arbitrary expression of depth 3
		//List<BoolExpression> lst = env.generateBool(1);
		List<Expression> lst = env.generateArith2(5);
		int size = lst.size();
		// the number of variable = 5
		// not optimized 71794821
		// optimized     55717221		
		// not optimized 5185404
		// optimized 4582900
		
		System.out.println(size);
//		for(int i = 0; i < size; i++){
//			lst.get(i).print();
//		}
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
	
	//@Test
	public void testOrder() {
		assertEquals(1, arith1.getOrder());
		assertEquals(1, arith2.getOrder());
		assertEquals(3, arith3.getOrder());
	}
	
	//@Test
	public void testContain() {
		assertEquals(true, arith3.equals(arith5));
		assertEquals(1, arith1.contain(1, varX));
		assertEquals(-1, arith2.contain(1, varY));
		assertEquals(-1, arith2.contain(-1, one));;
	}
	
	//@Test
	public void testNone() {
		Expression n1 = new None();
		Expression n2 = new None();
		BoolExpression bn1 = new BNone();
		BoolExpression bn2 = new BNone();
		assertEquals(true, n1.equals(n2));
		assertEquals(false, n1.equals(arith1));
		assertEquals(false, bn1.equals(bool1));
		assertEquals(true, bn1.equals(bn2));
	}	
	
	@Test
	public void testSynth() {
		File profile = new File("/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/Diffusion3d/profiles/profile1");
		File dir = new File("/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/Diffusion3d/profiles");
		File[] profiles = dir.listFiles();
		
		Synthesizer psysha = new Synthesizer();
		
		psysha.synthesizeFrom(profiles);
	}
}
