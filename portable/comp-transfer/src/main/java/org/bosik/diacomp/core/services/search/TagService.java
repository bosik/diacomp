package org.bosik.diacomp.core.services.search;

import java.util.Map;

public interface TagService
{
	/**
	 * Removes all tag data
	 */
	void reset();

	/**
	 * Increments tag
	 * 
	 * @param id
	 * @param value
	 */
	void incTag(String id, int value);

	Map<String, Integer> getTags();
}
