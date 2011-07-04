int bonzo(int val)
{
  int ret;
  cout << val << ' ';
  val = 2*val;
  cout << val << endl;
  if (val < 5) return 0;
  cout << "****\n";
  return val;
}
