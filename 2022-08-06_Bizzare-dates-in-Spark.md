# The bizzare world of parsing dates in Spark

Dates are famously hard to work with, but it turns out dates are even harder in Spark

## The numbers are off... but just slightly.

When I first stumbled across this it wasn't one of the bugs that just scream in your face - it was subtle.
Something didn't add up, and after a lot of digging it became clear that things didn't add up around the year mark - repeatedly, but weirdly enough
sometimes affecting a few days in December, and in other cases affecting a few days in January. But never both or neither.

Nothing unusal in the code, no warnings in the output - so what's going on?

## Date parsing in Spark

Let's first look at an example. We're just gonna parse some dates from strings in Spark, that can't be too hard.

![](/static/sparkWrongDates.png)

Turns out it is! That is quite a result though, it's not _super_ far off, just by about a  week... bizarre.

If we change the year placeholder from `YYYY` to `yyyy` things look like we want

![](/static/sparkDatesRight.png)


## Y isn't y... most of the times

Clearly Y is at fault here, but what on earth does it do?

Checking the [Spark docs](https://spark.apache.org/docs/latest/sql-ref-datetime-pattern.html) doesn't turn out to be terribly helpful either,
according to those `Y` doesn't even exist.

![](/static/Screenshot from 2022-08-05 22-41-03.png)

After some more digging it became clear that Spark uses
Java's [`SimpleDateFormat`](https://docs.oracle.com/javase/7/docs/api/java/text/SimpleDateFormat.html), and guess what - that one does have a `Y`.

![](/static/Screenshot from 2022-08-05 22-35-03.png)

And its called `Week Year`. Checking the docs linked above isn't actually terribly helpful, but [Wikipedia has a helpful description](https://en.wikipedia.org/wiki/ISO_8601#Week_dates) that started
to make sense after reading it 5 times.

__It boils down to that Week years are effectively week-aligned years. They don't started in the middle of the week, even though they might in reality.
Additional days are either assigned to January while they happen in December or vice-versa, depending on which requires fewer days to assign.__

Now, funnily enough, Python uses the `Y` in it's date format to a regular year, while a `y` means a year without the leading two century digits.

![](/static/Screenshot from 2022-08-05 22-55-08.png)

## Spark 3

Luckily! This is no longer the case with Spark 3. Spark 3 now implements it's own date parser, and the behaviour described above
is only possible if explicitely enabled.
