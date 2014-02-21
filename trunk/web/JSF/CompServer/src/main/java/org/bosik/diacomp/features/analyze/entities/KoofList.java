package org.bosik.diacomp.features.analyze.entities;

public class KoofList
{
	private static final int	MIN_PER_DAY	= 1440;

	private Koof[]				koofs;

	public KoofList()
	{
		koofs = new Koof[MIN_PER_DAY];
		for (int i = 0; i < MIN_PER_DAY; i++)
		{
			koofs[i] = new Koof();
		}
	}

	public Koof getKoof(int time)
	{
		return koofs[time];
	}
}
