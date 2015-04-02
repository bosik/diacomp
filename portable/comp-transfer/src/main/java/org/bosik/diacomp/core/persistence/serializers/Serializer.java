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
package org.bosik.diacomp.core.persistence.serializers;

import java.util.List;

/**
 * Converts <b>object(s) <—> string</b>
 * 
 * @param <T>Object type to be serialized / deserialized
 */
public interface Serializer<T>
{
	/**
	 * Deserializes object from string
	 * 
	 * @param s
	 *            String with serialized object
	 * @return Deserialized object (may be null if no data supplied)
	 */
	public T read(String s);

	/**
	 * Deserializes arbitrary amount of objects from string
	 * 
	 * @param s
	 *            String with serialized objects
	 * @return List of deserialized objects (may be empty if not data supplied)
	 */
	public List<T> readAll(String s);

	/**
	 * Serializes object into string
	 * 
	 * @param object
	 *            The object to be serialized
	 * @return String containing serialized object
	 */
	public String write(T object);

	/**
	 * Serializes arbitrary amount of objects into string
	 * 
	 * @param objects
	 *            List of objects to be serialized
	 * @return String containing serialized objects
	 */
	public String writeAll(List<T> objects);
}
