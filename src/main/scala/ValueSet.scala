package com.alecdorrington
package kneiphof

/**
  * An immutable set of values, which compares its elements only by equality.
  *
  * Unlike [[Set]], a value set is covariant, so a `ValueSet[Int]` is also a
  * `ValueSet[Any]`. Membership queries accept any value for the same reason.
  * It's stored as a hash trie from each element to itself, so membership,
  * insertion and removal take effectively constant time.
  */
final class ValueSet[+A] private (elements: Map[Any, A]) extends Iterable[A]:

  override def iterator: Iterator[A] = elements.valuesIterator

  override def knownSize: Int = elements.size

  /** @return `true` if and only if this set contains [[element]]. */
  def contains(element: Any): Boolean = elements.contains(element)

  /** @return The element of this set equal to [[element]], if there is one. */
  def get(element: Any): Option[A] = elements.get(element)

  /** @return This set with [[element]] added. */
  def incl[B >: A](element: B): ValueSet[B] =
    new ValueSet(elements.updated(element, element))

  /** Alias of [[incl]]. */
  infix def + [B >: A](element: B): ValueSet[B] = incl(element)

  /** @return This set without [[element]]. */
  def excl(element: Any): ValueSet[A] = new ValueSet(elements - element)

  /** Alias of [[excl]]. */
  infix def - (element: Any): ValueSet[A] = excl(element)

  /** @return The union of this set and [[that]]. */
  def union[B >: A](that: ValueSet[B]): ValueSet[B] =
    that.foldLeft(this: ValueSet[B])(_ + _)

  /** Alias of [[union]]. */
  infix def | [B >: A](that: ValueSet[B]): ValueSet[B] = union(that)

  override def filter(predicate: A => Boolean): ValueSet[A] =
    new ValueSet(elements.filter((_, element) => predicate(element)))

  override def equals(that: Any): Boolean = that match
    case that: ValueSet[?] => size == that.size && forall(that.contains)
    case _                 => false

  override def hashCode: Int = elements.keySet.##

  override def toString: String = mkString("ValueSet(", ", ", ")")

/** The companion object for [[ValueSet]]. */
object ValueSet:

  /** @return The set with no elements. */
  def empty[A]: ValueSet[A] = new ValueSet(Map.empty)

  /** @return The set of the given [[elements]]. */
  def apply[A](elements: A*): ValueSet[A] = from(elements)

  /** @return The set of the given [[elements]]. */
  def from[A](elements: IterableOnce[A]): ValueSet[A] = elements
    .iterator
    .foldLeft(empty[A])(_ + _)
