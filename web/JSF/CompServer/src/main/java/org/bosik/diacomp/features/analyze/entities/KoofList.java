package org.bosik.diacomp.features.analyze.entities;

import org.bosik.diacomp.core.utils.Utils;

public class KoofList
{
	private Koof[]				koofs;

	public KoofList()
	{
		koofs = new Koof[Utils.MinPerDay];
		for (int i = 0; i < Utils.MinPerDay; i++)
		{
			koofs[i] = new Koof();
		}
	}

	public Koof getKoof(int time)
	{
		return koofs[time];
	}
}
