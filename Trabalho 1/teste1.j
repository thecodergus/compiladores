double maior (double a, double b);
int fat (int n);
void imprimir(string s, double r);
void main();

double maior (double a, double b)
{
  int m;
  
  if (a > b) {m = a;}
  else {m = b;}
  return b;
}

int fat (int n)
{
  int f;
  
  f = 0;
  while (n > 0)
  { 
    f = f * n;
    n = n - 1;
  }
  return f;
}
   

void imprimir(string s, double r)
{
  int s;
  
  print (s);
  print (r);
  return 0;
}

void main(){
  int x, num;
  double a;
  int x;
  
  print("Numero:");
  read (num);
  x = fat (4);
  a = maior (2.5, 10);
  imprimir("teste:", 2);
  return 0;
}