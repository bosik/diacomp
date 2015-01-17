package org.bosik.diacomp.core.services.analyze;

import org.bosik.diacomp.core.services.analyze.entities.Koof;

public interface KoofService
{
	void update();

	Koof getKoof(int time);
}
