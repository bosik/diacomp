package org.bosik.compensation.bo.basic;

import java.util.Date;

public interface Versioned
{
	String getId();

	Date getTimeStamp();

	int getVersion();
}
