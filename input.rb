ruby

	puts "--------------------------";
	puts "> Prueba de operaciones:";
	puts "--------------------------";

	puts ">>> (1+(2+(3+4)));", (1+(2+(3+4)));
	puts ">>> ((1+2)+(3+4));", ((1+2)+(3+4));
	puts ">>> ((1+(2+3))+4);", ((1+(2+3))+4);
	puts ">>> ('Hola'+' mundo');", ("Hola"+" mundo");
	puts ">>> (['hola']+['mundo']);", (["hola"]+["mundo"]);
	puts ">>> ('hola'*2);", ("hola"*2);
	puts ">>> ([1,2]*2);", ([1,2]*2);

	puts "--------------------------";
	puts "> Pruebas del declare:";
	puts "--------------------------";

	puts ">>> declare a,b,c; a = 1; b = 2;";
	declare a,b,c;
	a = 1;
	b = 2;
	puts a, b, c;
	puts "--------------------------";
	puts "> Pruebas val + function exp:";
	puts "--------------------------";
	def fact(n)
	    if(n == 0 ) then
	        return 1;
	    else
	        return (n * (fact((n - 1))));
	    end
	end
	puts ">>> fact(5)",(fact(5));

	def fibo(n)
		if (n <= 2) then
			return 1;
		else
			declare left, right;
			left = (n - 1);
			right = (n - 2);
			return ( (fibo (left)) + (fibo (right)) );
		end
	end
	puts ">>> fibo(10)",(fibo(10));

	def to_text(num)
		declare text;
		text = [
			"cero", "uno",
			"dos", "tres",
			"cuatro", "cinco",
			"seis", "siete",
			"ocho", "nueve"
		];
		return (text[num]);
	end
	puts ">>> to_text(5)",(to_text(5));

	def hola()
		return "mundo";
	end
	#puts ">>> hola", hola;
	puts ">>> puts hola; [ ESTA PRUEBA OPACA A LAS OTRAS, POR LO TANTO SE DEJA COMENTADA PERO DISPONIBLE PARA PROBAR. ]";

	puts "--------------------------";
	puts "> Pruebas if  + unless exp:";
	puts "--------------------------";

	def enIntervalo(min,max,value)
		if (min < value) then
			if (value < max) then
				return true;
			end
		else
			return false;
		end
	end

	declare w;
	w = enIntervalo(1, 10, 5);
	if w then
		puts ">>> if: En el intervalo: valor 5, mínimo 1, máximo 10";
	end

	w = enIntervalo(1, 10, 11);
	unless w then
		puts ">>> unless: Fuera del intervalo: valor 11, mínimo 1, máximo 10";
	end

	puts ">>> elseif: if(x <= 0) con x = 0: Fuera del intervalo: valor 11, mínimo 1, máximo 10";
	declare x;
	x = 0;
	if (x <= 0) then
		puts "Entra al if" ;
	elsif (x > 0 ) then
		puts "Entra al elseif" ;
	end

	puts ">>> elseif: elseif(x > 0) con x = 2: Fuera del intervalo: valor 11, mínimo 1, máximo 10";
	declare x;
	x = 2;
	if (x <= 0) then
		puts "Entra al if" ;
	elsif (x > 0 ) then
		puts "Entra al elseif" ;
	end

	puts "--------------------------";
	puts "> Pruebas while  + for exp:";
	puts "--------------------------";

	puts ">>> while( x < 3 ) con x = 0";
	declare x;
	x = 0;
	while( x < 3 ) do
		puts x;
		x += 1;
	end

	puts ">>> until( x >= 3 ) con x = 0";
	declare x;
	x = 0;
	until( x >= 3 ) do
		puts x;
		x += 1;
	end

	puts ">>> for x in (0..2)";
	for x in (0..2) do
		puts x;
	end

	puts ">>> for x in ((0..5) step 2)";
	for x in ((0..5) step 2) do
		puts x;
	end

	puts ">>> for x in ['Hola', 'mundo']";
	for x in ["Hola", "mundo"] do
		puts x;
	end

	puts ">>> for x in a con a = ['Hola', 'mundo']";
	declare a;
	a = ["Hola", "mundo"];
	for x in a do
		puts x;
	end

end