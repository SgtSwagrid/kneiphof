package com.alecdorrington
package kneiphof

// Helpers shared across the library, which aren't part of its public API.

/** @return The ways of choosing one element from each of the [[options]]. */
private[kneiphof] def cartesian[A](options: Seq[Iterable[A]]): Seq[Seq[A]] =
  options.foldRight(Seq(Seq.empty[A])): (option, rest) =>
    for
      head <- option.toSeq
      tail <- rest
    yield head +: tail

/**
  * @return
  *   The text [[text]], followed by [[label]] unless the label is `()`.
  */
private[kneiphof] def describe(text: String, label: Any): String = label match
  case () => text
  case _  => s"$text % $label"
