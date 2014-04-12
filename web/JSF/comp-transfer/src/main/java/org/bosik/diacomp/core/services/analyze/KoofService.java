package org.bosik.diacomp.core.services.analyze;

import java.util.Date;
import org.bosik.diacomp.core.services.analyze.entities.Koof;

public interface KoofService
{
	void setTimeRange(Date timeFrom, Date timeTo);

	void update();

	Koof getKoof(int time);
}
