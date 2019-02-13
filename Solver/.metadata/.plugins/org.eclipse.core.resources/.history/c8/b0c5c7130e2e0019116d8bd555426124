package Expression;

import java.io.File;

public class Main {
	public static void main(String[] args){
		File file1 = new File("/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/himenoBMT/profile");
		File file2 = new File("/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/Matrixmultiply/profile");
		File file3 = new File("/Users/akira/masuhara-lab/Kani-CUDA/Emulator/Examples/Diffusion3d/profile");
		long start = System.currentTimeMillis();
		Synthesizer kani_cuda = new Synthesizer();
		kani_cuda.input(file3);
		
		
		kani_cuda.synthesizeBool().print();;
		//long end = System.currentTimeMillis();
		//System.out.println("time: " + (end - start) + "ms");
		
		//start = System.currentTimeMillis();
		kani_cuda.synthesizeArith(3).print();
		kani_cuda.synthsizeIf().print();;
		long end = System.currentTimeMillis();
		
		System.out.println("time: " + (end - start) + "ms");
	}
}
