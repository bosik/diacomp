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
package org.bosik.diacomp.web.backend.features.base.food.combo;

public class Api
{
	public static final String BASE_URL = "/food";

	public static class Count
	{
		public static final String URL = BASE_URL + "/count";
	}

	public static class FindById
	{
		public static final String URL = BASE_URL + "/guid";
	}

	public static class FindAny
	{
		public static final String URL          = BASE_URL + "/search";
		public static final String PARAM_FILTER = "q";
	}

	public static class FindAll
	{
		public static final String URL                   = BASE_URL + "/all";
		public static final String PARAM_INCLUDE_REMOVED = "show_rem";
	}

	public static class FindChanged
	{
		public static final String URL         = BASE_URL + "/changes";
		public static final String PARAM_SINCE = "since";
	}

	public static class Hash
	{
		public static final String URL = BASE_URL + "/hash";
	}

	public static class Hashes
	{
		public static final String URL = BASE_URL + "/hashes";
	}

	public static class Save
	{
		public static final String URL         = BASE_URL;
		public static final String PARAM_DATA  = "items";
		public static final String RESPONSE_OK = "Saved OK";
	}
}
