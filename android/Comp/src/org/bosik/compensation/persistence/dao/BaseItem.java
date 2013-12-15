package org.bosik.compensation.persistence.dao;

import org.bosik.compensation.bo.RelativeTagged;
import org.bosik.compensation.bo.basic.TrueCloneable;

public interface BaseItem extends RelativeTagged, TrueCloneable
{
	String getId();
}
