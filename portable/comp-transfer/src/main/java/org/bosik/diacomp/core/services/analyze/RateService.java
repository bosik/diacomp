/*
 * Diacomp - Diabetes analysis & management system
 * Copyright (C) 2013 Nikita Bosik
 * 
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.bosik.diacomp.core.services.analyze;

import org.bosik.diacomp.core.services.analyze.entities.Rate;

public interface RateService
{
	/**
	 * Refreshes rates. This operation may be time/memory consuming.
	 */
	void update();

	/**
	 * Fetches rate for specified time of day
	 *
	 * @param time Time in minutes
	 * @return Rate if found, {code null} otherwise
	 */
	Rate getRate(int time);
}
