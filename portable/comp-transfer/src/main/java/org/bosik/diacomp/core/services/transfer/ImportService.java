package org.bosik.diacomp.core.services.transfer;

import java.io.IOException;
import java.io.InputStream;

public interface ImportService
{
	void importData(InputStream stream) throws IOException;
}
