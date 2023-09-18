namespace Compiler
{
    public class Compiler
    {
        public static void Main()
        {
            string str = File.ReadAllText("program.floh");

            Console.Write("Tokenizing...");
            Tokenizer tokenizer = new(str);
            List<Token> tokens = tokenizer.Tokenize();
            Console.WriteLine("Success!");

            Console.Write("Parsing...");
            Parser parser = new(tokens);
            NodeProg prog = parser.Parse();
            Console.WriteLine("Success!");

            Console.Write("Generating...");
            Generator generator = new(prog);
            string output = generator.Generate();
            Console.WriteLine("Success!");

            File.WriteAllText("out.asm", output);
        }

        public static void Error(string errorMessage, int line = -1)
        {
            if (line != -1)
            {
                Console.Write($"\n[Line {line}: Error] " + errorMessage);
            }
            else
            {
                Console.Write($"\n[Error] " + errorMessage);
            }
            Environment.Exit(1);
        }
    }
}