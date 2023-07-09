struct S
{
	int myProp() @property
	{
		static if (a)
		{
		}
		else if (b)
		{
		}
	}
}
