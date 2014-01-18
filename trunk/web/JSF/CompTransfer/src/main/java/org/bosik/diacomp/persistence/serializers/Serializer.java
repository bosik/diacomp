package org.bosik.diacomp.persistence.serializers;

import java.util.List;

/**
 * Converts <b>object(s) <—> string</b>
 * 
 * @author Bosik
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
	 * @return Deserialized object
	 */
	public T read(String s);

	/**
	 * Deserializes arbitrary amount of objects from string
	 * 
	 * @param s
	 *            String with serialized objects
	 * @return List of deserialized objects
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